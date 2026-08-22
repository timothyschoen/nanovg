//
// nanovg_async.cpp
//
// Pool management, frame hand-off and command replay for the async layer.
// See nanovg_async.h for the design overview.
//

#include "nanovg_async.h"

namespace nanovg {


// ---------------------------------------------------------------------------
// Frame hand-off (producer thread).
//
// Publishing is a couple of pointer moves under a very short lock:
//   * the freshly recorded buffer becomes `pending`
//   * any previous, still-unconsumed `pending` is recycled (that frame is
//     coalesced away so the consumer always sees the freshest frame)
//   * a recycled buffer is taken from the free list to record the next frame
//
// A pool of >= 3 buffers guarantees the free list is never empty at this point,
// so the producer neither blocks nor allocates in steady state.
// ---------------------------------------------------------------------------
void Context::publish()
{
    CommandBuffer* next = nullptr;
    {
        std::lock_guard<std::mutex> lock(mutex);

        // Commit this frame's resource ops together with the buffer swap, so the
        // consumer applies them with the frame that uses them (never ahead of the
        // still-pending previous frame). Coalescing a frame keeps its resource ops
        // in `committedResources`, so creates/deletes stay in order and are not lost.
        if (!recordingResources.empty()) {
            committedResources.insert(committedResources.end(),
                std::make_move_iterator(recordingResources.begin()),
                std::make_move_iterator(recordingResources.end()));
            recordingResources.clear();
        }

        if (pending)
            freeList.push_back(pending);   // drop the previous unconsumed frame
        pending = record;

        if (!freeList.empty()) {
            next = freeList.back();
            freeList.pop_back();
        }
    }

    if (!next) {
        // Only reachable if the pool was under-sized; keep correctness anyway.
        next = new CommandBuffer(1u << 16);
        std::lock_guard<std::mutex> lock(mutex);
        ownedBuffers.push_back(next);
    }

    next->reset();
    record = next;
}

bool hasPendingFrame(NVGcontext* handle)
{
    auto* x = reinterpret_cast<Context*>(handle);
    std::lock_guard<std::mutex> lock(x->mutex);
    return x->pending != nullptr;
}

int Context::allocateImageId(int const width, int const height, int const bytesPerPixel)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    int const image = nextImageId++;
    images[image] = { 0, width, height, bytesPerPixel };
    return image;
}

void Context::enqueueResource(ResourceOp&& op)
{
    // Producer thread only: stage into the current frame. These are committed to
    // the consumer at publish(), atomically with the buffer swap.
    recordingResources.push_back(std::move(op));
}

// Apply a batch of resource ops (create/update/delete) to the real context. Runs
// on the consumer thread with the batch grabbed atomically with the frame that
// uses them, so images are created/updated/deleted in order, never lost to
// coalescing, and never deleted ahead of a still-pending frame that uses them.
void Context::applyResources(std::vector<ResourceOp>& ops)
{
    NVGcontext* const R = real;
    for (auto& op : ops) {
        switch (op.kind) {
        case ResourceOp::Kind::CreateARGB: {
            int const realId = ::nvgCreateImageARGB(R, op.width, op.height, op.flags, op.data.empty() ? nullptr : op.data.data());
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[op.image];
            info.width = op.width; info.height = op.height; info.bytesPerPixel = 4; info.realId = realId;
        } break;
        case ResourceOp::Kind::CreateARGBSRGB: {
            int const realId = ::nvgCreateImageARGB_sRGB(R, op.width, op.height, op.flags, op.data.empty() ? nullptr : op.data.data());
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[op.image];
            info.width = op.width; info.height = op.height; info.bytesPerPixel = 4; info.realId = realId;
        } break;
        case ResourceOp::Kind::CreateAlpha: {
            int const realId = ::nvgCreateImageAlpha(R, op.width, op.height, op.flags, op.data.empty() ? nullptr : op.data.data());
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[op.image];
            info.width = op.width; info.height = op.height; info.bytesPerPixel = 1; info.realId = realId;
        } break;
        case ResourceOp::Kind::Update: {
            int realId = 0;
            {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = images.find(op.image);
                if (iter != images.end())
                    realId = iter->second.realId;
            }
            if (realId != 0 && !op.data.empty())
                ::nvgUpdateImage(R, realId, op.data.data());
        } break;
        case ResourceOp::Kind::Delete: {
            int realId = 0;
            {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = images.find(op.image);
                if (iter != images.end()) {
                    realId = iter->second.realId;
                    images.erase(iter);
                }
            }
            if (realId != 0)
                ::nvgDeleteImage(R, realId);
        } break;
        case ResourceOp::Kind::CreateFramebuffer: {
            NVGframebuffer* const realFramebuffer = nvgCreateFramebuffer(R, op.width, op.height, op.flags);
            int const realImage = realFramebuffer ? nvgFramebufferImage(realFramebuffer) : 0;
            bool keepFramebuffer = false;
            {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = framebuffers.find(op.framebuffer);
                if (iter != framebuffers.end()) {
                    iter->second.real = realFramebuffer;
                    iter->second.width = op.width;
                    iter->second.height = op.height;
                    iter->second.imageFlags = op.flags;
                    images[iter->second.virtualImageId].realId = realImage;
                    keepFramebuffer = true;
                }
            }
            // Its delete already ran (out-of-order should not happen, but stay safe).
            if (!keepFramebuffer && realFramebuffer)
                nvgDeleteFramebuffer(realFramebuffer);
        } break;
        case ResourceOp::Kind::DeleteFramebuffer: {
            NVGframebuffer* realFramebuffer = nullptr;
            bool releaseToken = false;
            {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = framebuffers.find(op.framebuffer);
                if (iter != framebuffers.end()) {
                    realFramebuffer = iter->second.real;
                    images.erase(iter->second.virtualImageId);
                    framebuffers.erase(iter);
                    releaseToken = true;
                }
            }
            if (realFramebuffer)
                nvgDeleteFramebuffer(realFramebuffer);
            if (releaseToken)
                delete reinterpret_cast<uint64_t*>(op.framebuffer);
        } break;
        case ResourceOp::Kind::FramebufferPass:
            // The create that made this pass's target ran earlier in this same
            // ordered batch, so its real FBO (bound inside the pass) already exists.
            if (op.pass)
                replayBuffer(*op.pass);
            break;
        }
    }
}

void* Context::allocateFramebuffer(int const width, int const height, int const imageFlags)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    auto* framebuffer = new uint64_t(nextFramebufferId++);
    int const image = nextImageId++;
    framebuffers[framebuffer] = { nullptr, image, width, height, imageFlags };
    images[image] = { 0, width, height, 4 };
    return framebuffer;
}

int Context::framebufferImage(void* framebuffer) const
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    auto const iter = framebuffers.find(framebuffer);
    return iter == framebuffers.end() ? 0 : iter->second.virtualImageId;
}

void* Context::underlyingFramebuffer(void* framebuffer) const
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    auto const iter = framebuffers.find(framebuffer);
    void* real = iter == framebuffers.end() ? nullptr : iter->second.real;
    return real;
}

// Producer thread: hand out a path id. This ONLY assigns the number; the id is
// not marked "cached" here. A cached path is durable state on the real context
// (nvgSavePath stores it in a persistent map), created at replay time -- and
// that replay can be dropped by frame coalescing. So membership in `paths` is
// owned by the CONSUMER (see confirmPathSaved / confirmPathDeleted): it is set
// only once the real nvgSavePath has actually run. If we marked the path cached
// here instead, a coalesced-away SavePath would leave the producer believing a
// glyph is cached when the real context never saved it -> checkPathId returns
// true, drawGlyphs emits only FillCachedPath, and the glyph silently vanishes.
uint32_t Context::allocatePathId(uint32_t pathId)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    return pathId == static_cast<uint32_t>(-1) ? nextPathId++ : pathId;
}

// Consumer thread: the real nvgSavePath for `pathId` just succeeded, so the path
// now genuinely exists in the real context's cache.
void Context::confirmPathSaved(uint32_t pathId)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    paths.insert(pathId);
}

// Consumer thread: the real nvgDeletePath for `pathId` just ran.
void Context::confirmPathDeleted(uint32_t pathId)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    paths.erase(pathId);
}

// Producer thread: is this path known to exist in the real context's cache?
// Reflects consumer-confirmed state, so it stays false until a SavePath has
// actually been replayed -- surviving coalescing.
bool Context::checkPathId(uint32_t pathId)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    return paths.contains(pathId);
}

// Consumer thread: the real nvgSaveSDFGlyph for `hash` just succeeded.
void Context::confirmSDFGlyphSaved(uint64_t hash)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    sdfGlyphs.insert(hash);
}

// Producer thread: is this glyph's SDF tile known to exist on the real context?
bool Context::checkSDFGlyph(uint64_t hash)
{
    std::lock_guard<std::mutex> lock(resourceMutex);
    return sdfGlyphs.contains(hash);
}

int Context::resolveImageId(int const image) const
{
    if (image == 0)
        return 0;
    std::lock_guard<std::mutex> lock(resourceMutex);
    auto const iter = images.find(image);
    // Never hand a virtual id to the GL backend: it indexes gl->textures[id-1]
    // and would read far out of bounds. An unknown image, or one whose real
    // texture has not been created yet, resolves to 0 (no texture) instead of
    // crashing. With the resource queue drained before every replay this only
    // trips if something is genuinely wrong -- it is a safety net, not a hot path.
    return iter == images.end() ? 0 : iter->second.realId;
}

NVGpaint Context::resolvePaint(NVGpaint paint) const
{
    if (paint.image != 0)
        paint.image = resolveImageId(paint.image);

    return paint;
}

void* createFramebuffer(NVGcontext* c, int width, int height, int imageFlags)
{
    auto* x = detail::ctx(c);
    // Eagerly reserve the key + backing virtual image id so the producer can build
    // paints against it in the same frame; the real FBO is created when the op runs.
    void* const framebuffer = x->allocateFramebuffer(width, height, imageFlags);

    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::CreateFramebuffer;
    op.framebuffer = framebuffer;
    op.width = width;
    op.height = height;
    op.flags = imageFlags;
    x->enqueueResource(std::move(op));
    return framebuffer;
}

void deleteFramebuffer(NVGcontext* c, void* framebuffer)
{
    if (framebuffer == nullptr)
        return;

    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::DeleteFramebuffer;
    op.framebuffer = framebuffer;
    detail::ctx(c)->enqueueResource(std::move(op));
}

void commitFramebufferPass(NVGcontext* c, void* framebuffer, std::function<void(NVGcontext*)> body)
{
    auto* x = detail::ctx(c);

    // Record bind(target) -> body -> unbind into a standalone buffer, off the
    // current frame. Its own frame boundaries (nvgBeginFrame / EndFrame) live
    // inside `body`; it must NOT publish (see the header contract).
    auto pass = std::make_unique<CommandBuffer>(1u << 12);
    {
        ScopedCommandRecorder recorder(c, *pass);
        bindFramebuffer(c, framebuffer);
        body(c);
        bindFramebuffer(c, nullptr);
    }

    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::FramebufferPass;
    op.pass = std::move(pass);
    x->enqueueResource(std::move(op));
}

void bindFramebuffer(NVGcontext* c, void* framebuffer)
{
    auto& b = detail::rec(c);
    b.putOp(Op::BindFramebuffer);
    b.put(reinterpret_cast<uintptr_t>(framebuffer));
}

void setMainFramebuffer(NVGcontext* c, NVGframebuffer* realFramebuffer)
{
    // Consumer thread only: no lock needed (mainTarget is touched only here and at
    // Op::BindMainFramebuffer replay, both on the GL thread).
    detail::ctx(c)->mainTarget = realFramebuffer;
}

void bindMainFramebuffer(NVGcontext* c)
{
    detail::rec(c).putOp(Op::BindMainFramebuffer);
}

void viewport(NVGcontext* c, int x, int y, int width, int height)
{
    auto& b = detail::rec(c);
    b.putOp(Op::Viewport);
    b.put(x);
    b.put(y);
    b.put(width);
    b.put(height);
}

void clear(NVGcontext* c)
{
    detail::rec(c).putOp(Op::Clear);
}

int framebufferImage(NVGcontext* c, void* framebuffer)
{
    return detail::ctx(c)->framebufferImage(framebuffer);
}

void* underlyingFramebuffer(NVGcontext* c, void* framebuffer)
{
    return detail::ctx(c)->underlyingFramebuffer(framebuffer);
}

// ---------------------------------------------------------------------------
// Replay (consumer thread). Reads args in the exact order they were written and
// forwards to the real NanoVG functions.
// ---------------------------------------------------------------------------
void Context::replayBuffer(CommandBuffer const& buf)
{
    NVGcontext* const R = real;
    size_t pos = 0;
    size_t const end = buf.size();

    while (pos < end) {
        Op const op = buf.getOp(pos);
        switch (op) {

        // --- frame ---
        case Op::BeginFrame: {
            float w = buf.get<float>(pos), h = buf.get<float>(pos), dpr = buf.get<float>(pos);
            ::nvgBeginFrame(R, w, h, dpr);
        } break;
        case Op::EndFrame:
            ::nvgEndFrame(R);
            break;

        // --- backend framebuffers ---
        case Op::BindFramebuffer: {
            auto* key = reinterpret_cast<void*>(buf.get<uintptr_t>(pos));
            NVGframebuffer* realFramebuffer = nullptr;
            if (key != nullptr) {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = framebuffers.find(key);
                if (iter != framebuffers.end()) {
                    realFramebuffer = iter->second.real;
                    // The CreateFramebuffer resource op runs before any frame that
                    // binds this key, so `real` is normally already set. Kept as a
                    // defensive fallback (e.g. a bind reached without its create).
                    if (!realFramebuffer) {
                        realFramebuffer = nvgCreateFramebuffer(R, iter->second.width, iter->second.height, iter->second.imageFlags);
                        iter->second.real = realFramebuffer;
                        int const realImage = realFramebuffer ? nvgFramebufferImage(realFramebuffer) : 0;
                        images[iter->second.virtualImageId].realId = realImage;
                    }
                }
            }

            nvgBindFramebuffer(realFramebuffer);
        } break;
        case Op::BindMainFramebuffer:
            // Bind the surface's persistent main/damage framebuffer, which the GL
            // thread owns and set via setMainFramebuffer() before this replay.
            nvgBindFramebuffer(mainTarget);
            break;
        case Op::Viewport: {
            int const x = buf.get<int>(pos), y = buf.get<int>(pos), w = buf.get<int>(pos), h = buf.get<int>(pos);
            nvgViewport(x, y, w, h);
        } break;
        case Op::Clear:
            nvgClear(R);
            break;
        case Op::RenderCallback: {
            auto const callback = buf.get<RenderCallback>(pos);
            uint32_t len = 0;
            auto const* data = buf.getBytes(pos, len);
            if (callback != nullptr)
                callback(R, data, len);
        } break;
        case Op::RenderOwnedCallback:
            buf.callOwnedRenderCallback(R, pos);
            break;

        // --- composite ---
        case Op::GlobalCompositeOperation:
            ::nvgGlobalCompositeOperation(R, buf.get<int>(pos));
            break;
        case Op::GlobalCompositeBlendFunc: {
            int s = buf.get<int>(pos), d = buf.get<int>(pos);
            ::nvgGlobalCompositeBlendFunc(R, (NVGblendFactor)s, (NVGblendFactor)d);
        } break;
        case Op::GlobalCompositeBlendFuncSeparate: {
            int sr = buf.get<int>(pos), dr = buf.get<int>(pos), sa = buf.get<int>(pos), da = buf.get<int>(pos);
            ::nvgGlobalCompositeBlendFuncSeparate(R, (NVGblendFactor)sr, (NVGblendFactor)dr, (NVGblendFactor)sa, (NVGblendFactor)da);
        } break;

        // --- state ---
        case Op::Save:    ::nvgSave(R); break;
        case Op::Restore: ::nvgRestore(R); break;
        case Op::Reset:   ::nvgReset(R); break;

        // --- style ---
        case Op::ShapeAntiAlias: ::nvgShapeAntiAlias(R, buf.get<int>(pos)); break;
        case Op::StrokeColor:    ::nvgStrokeColor(R, buf.get<NVGcolor>(pos)); break;
        case Op::StrokePaint:    ::nvgStrokePaint(R, resolvePaint(buf.get<NVGpaint>(pos))); break;
        case Op::FillColor:      ::nvgFillColor(R, buf.get<NVGcolor>(pos)); break;
        case Op::FillPaint:      ::nvgFillPaint(R, resolvePaint(buf.get<NVGpaint>(pos))); break;
        case Op::MiterLimit:     ::nvgMiterLimit(R, buf.get<float>(pos)); break;
        case Op::StrokeWidth:    ::nvgStrokeWidth(R, buf.get<float>(pos)); break;
        case Op::LineStyle:      ::nvgLineStyle(R, buf.get<int>(pos)); break;
        case Op::DashLength:     ::nvgDashLength(R, buf.get<float>(pos)); break;
        case Op::DashPhaseOffset: ::nvgDashPhaseOffset(R, buf.get<float>(pos)); break;
        case Op::LineCap:        ::nvgLineCap(R, buf.get<int>(pos)); break;
        case Op::LineJoin:       ::nvgLineJoin(R, buf.get<int>(pos)); break;
        case Op::GlobalAlpha:    ::nvgGlobalAlpha(R, buf.get<float>(pos)); break;

        // --- transforms ---
        case Op::ResetTransform: ::nvgResetTransform(R); break;
        case Op::Transform: {
            float a = buf.get<float>(pos), b = buf.get<float>(pos), c = buf.get<float>(pos);
            float d = buf.get<float>(pos), e = buf.get<float>(pos), f = buf.get<float>(pos);
            ::nvgTransform(R, a, b, c, d, e, f);
        } break;
        case Op::Translate: { float x = buf.get<float>(pos), y = buf.get<float>(pos); ::nvgTranslate(R, x, y); } break;
        case Op::Rotate:    ::nvgRotate(R, buf.get<float>(pos)); break;
        case Op::SkewX:     ::nvgSkewX(R, buf.get<float>(pos)); break;
        case Op::SkewY:     ::nvgSkewY(R, buf.get<float>(pos)); break;
        case Op::Scale:     { float x = buf.get<float>(pos), y = buf.get<float>(pos); ::nvgScale(R, x, y); } break;
        case Op::TransformQuantize: ::nvgTransformQuantize(R); break;

        // --- scissor ---
        case Op::GlobalScissor: {
            int x = buf.get<int>(pos), y = buf.get<int>(pos), w = buf.get<int>(pos), h = buf.get<int>(pos);
            ::nvgGlobalScissor(R, x, y, w, h);
        } break;
        case Op::Scissor: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            ::nvgScissor(R, x, y, w, h);
        } break;
        case Op::RoundedScissor: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgRoundedScissor(R, x, y, w, h, r);
        } break;
        case Op::IntersectScissor: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            ::nvgIntersectScissor(R, x, y, w, h);
        } break;
        case Op::IntersectRoundedScissor: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgIntersectRoundedScissor(R, x, y, w, h, r);
        } break;
        case Op::ResetScissor: ::nvgResetScissor(R); break;

        // --- paths ---
        case Op::BeginPath: ::nvgBeginPath(R); break;
        case Op::MoveTo: { float x = buf.get<float>(pos), y = buf.get<float>(pos); ::nvgMoveTo(R, x, y); } break;
        case Op::LineTo: { float x = buf.get<float>(pos), y = buf.get<float>(pos); ::nvgLineTo(R, x, y); } break;
        case Op::BezierTo: {
            float c1x = buf.get<float>(pos), c1y = buf.get<float>(pos), c2x = buf.get<float>(pos);
            float c2y = buf.get<float>(pos), x = buf.get<float>(pos), y = buf.get<float>(pos);
            ::nvgBezierTo(R, c1x, c1y, c2x, c2y, x, y);
        } break;
        case Op::QuadTo: {
            float cx = buf.get<float>(pos), cy = buf.get<float>(pos), x = buf.get<float>(pos), y = buf.get<float>(pos);
            ::nvgQuadTo(R, cx, cy, x, y);
        } break;
        case Op::ArcTo: {
            float x1 = buf.get<float>(pos), y1 = buf.get<float>(pos), x2 = buf.get<float>(pos), y2 = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgArcTo(R, x1, y1, x2, y2, r);
        } break;
        case Op::ClosePath: ::nvgClosePath(R); break;
        case Op::PathWinding: ::nvgPathWinding(R, (NVGwinding)buf.get<int>(pos)); break;
        case Op::Arc: {
            float cx = buf.get<float>(pos), cy = buf.get<float>(pos), r = buf.get<float>(pos);
            float a0 = buf.get<float>(pos), a1 = buf.get<float>(pos); int dir = buf.get<int>(pos);
            ::nvgArc(R, cx, cy, r, a0, a1, dir);
        } break;
        case Op::Rect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            ::nvgRect(R, x, y, w, h);
        } break;
        case Op::RoundedRect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgRoundedRect(R, x, y, w, h, r);
        } break;
        case Op::RoundedRectVarying: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            float tl = buf.get<float>(pos), tr = buf.get<float>(pos), br = buf.get<float>(pos), bl = buf.get<float>(pos);
            ::nvgRoundedRectVarying(R, x, y, w, h, tl, tr, br, bl);
        } break;
        case Op::Ellipse: {
            float cx = buf.get<float>(pos), cy = buf.get<float>(pos), rx = buf.get<float>(pos), ry = buf.get<float>(pos);
            ::nvgEllipse(R, cx, cy, rx, ry);
        } break;
        case Op::Circle: {
            float cx = buf.get<float>(pos), cy = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgCircle(R, cx, cy, r);
        } break;
        case Op::Fill:   ::nvgFill(R); break;
        case Op::Stroke: ::nvgStroke(R); break;

        // --- cached paths ---
        case Op::SavePath: {
            uint32_t const pathId = buf.get<uint32_t>(pos);
            // nvgSavePath returns -1 (and stores nothing) for an empty path; only
            // mark the id cached when the real save actually happened.
            if (::nvgSavePath(R, pathId) != -1)
                confirmPathSaved(pathId);
        } break;
        case Op::StrokeCachedPath: ::nvgStrokeCachedPath(R, buf.get<uint32_t>(pos)); break;
        case Op::FillCachedPath:   ::nvgFillCachedPath(R, buf.get<uint32_t>(pos)); break;
        case Op::DeletePath: {
            uint32_t const pathId = buf.get<uint32_t>(pos);
            ::nvgDeletePath(R, pathId);
            confirmPathDeleted(pathId);
        } break;

        // --- image resources ---
        case Op::CreateImageARGB: {
            int image = buf.get<int>(pos), w = buf.get<int>(pos), h = buf.get<int>(pos), flags = buf.get<int>(pos);
            uint32_t len = 0; auto const* data = reinterpret_cast<unsigned char const*>(buf.getBytes(pos, len));
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[image];
            info.width = w; info.height = h; info.bytesPerPixel = 4;
            info.realId = ::nvgCreateImageARGB(R, w, h, flags, len ? data : nullptr);
        } break;
        case Op::CreateImageARGBSRGB: {
            int image = buf.get<int>(pos), w = buf.get<int>(pos), h = buf.get<int>(pos), flags = buf.get<int>(pos);
            uint32_t len = 0; auto const* data = reinterpret_cast<unsigned char const*>(buf.getBytes(pos, len));
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[image];
            info.width = w; info.height = h; info.bytesPerPixel = 4;
            info.realId = ::nvgCreateImageARGB_sRGB(R, w, h, flags, len ? data : nullptr);
        } break;
        case Op::CreateImageAlpha: {
            int image = buf.get<int>(pos), w = buf.get<int>(pos), h = buf.get<int>(pos), flags = buf.get<int>(pos);
            uint32_t len = 0; auto const* data = reinterpret_cast<unsigned char const*>(buf.getBytes(pos, len));
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto& info = images[image];
            info.width = w; info.height = h; info.bytesPerPixel = 1;
            info.realId = ::nvgCreateImageAlpha(R, w, h, flags, len ? data : nullptr);
        } break;
        case Op::UpdateImage: {
            int image = buf.get<int>(pos);
            uint32_t len = 0; auto const* data = reinterpret_cast<unsigned char const*>(buf.getBytes(pos, len));
            if (len != 0) {
                std::lock_guard<std::mutex> lock(resourceMutex);
                auto const iter = images.find(image);
                ::nvgUpdateImage(R, iter == images.end() || iter->second.realId == 0 ? image : iter->second.realId, data);
            }
        } break;
        case Op::DeleteImage: {
            int image = buf.get<int>(pos);
            std::lock_guard<std::mutex> lock(resourceMutex);
            auto const iter = images.find(image);
            if (iter != images.end()) {
                if (iter->second.realId != 0)
                    ::nvgDeleteImage(R, iter->second.realId);
                images.erase(iter);
            } else {
                ::nvgDeleteImage(R, image);
            }
        } break;

        // --- text ---
        case Op::Text: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos);
            uint32_t len = 0; char const* s = buf.getBytes(pos, len);
            ::nvgText(R, x, y, s, s + len);
        } break;
        case Op::TextBox: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), brw = buf.get<float>(pos);
            uint32_t len = 0; char const* s = buf.getBytes(pos, len);
            ::nvgTextBox(R, x, y, brw, s, s + len);
        } break;
        case Op::FontSize:          ::nvgFontSize(R, buf.get<float>(pos)); break;
        case Op::TextLetterSpacing: ::nvgTextLetterSpacing(R, buf.get<float>(pos)); break;
        case Op::TextLineHeight:    ::nvgTextLineHeight(R, buf.get<float>(pos)); break;
        case Op::TextAlign:         ::nvgTextAlign(R, buf.get<int>(pos)); break;
        case Op::AtlasTextThreshold: ::nvgAtlasTextThreshold(R, buf.get<float>(pos)); break;
        case Op::FontFaceId:        ::nvgFontFaceId(R, buf.get<int>(pos)); break;
        case Op::FontFace: {
            uint32_t len = 0; char const* s = buf.getBytes(pos, len);
            if (len > 0) ::nvgFontFace(R, s);   // s is null-terminated (terminator was stored)
        } break;

        // --- plugdata direct draws ---
        case Op::FillRect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            ::nvgFillRect(R, x, y, w, h);
        } break;
        case Op::StrokeRect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            ::nvgStrokeRect(R, x, y, w, h);
        } break;
        case Op::DrawRoundedRect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            NVGcolor ic = buf.get<NVGcolor>(pos), oc = buf.get<NVGcolor>(pos); float r = buf.get<float>(pos);
            ::nvgDrawRoundedRect(R, x, y, w, h, ic, oc, r);
        } break;
        case Op::DrawObjectWithFlag: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            NVGcolor ic = buf.get<NVGcolor>(pos), oc = buf.get<NVGcolor>(pos), fc = buf.get<NVGcolor>(pos);
            float r = buf.get<float>(pos); int ft = buf.get<int>(pos); uint8_t fo = buf.get<uint8_t>(pos);
            ::nvgDrawObjectWithFlag(R, x, y, w, h, ic, oc, fc, r, (ObjectFlagType)ft, fo != 0);
        } break;
        case Op::FillRoundedRect: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos), r = buf.get<float>(pos);
            ::nvgFillRoundedRect(R, x, y, w, h, r);
        } break;
        case Op::SmoothGlow: {
            float x = buf.get<float>(pos), y = buf.get<float>(pos), w = buf.get<float>(pos), h = buf.get<float>(pos);
            NVGcolor ic = buf.get<NVGcolor>(pos), oc = buf.get<NVGcolor>(pos); float r = buf.get<float>(pos), fe = buf.get<float>(pos);
            ::nvgSmoothGlow(R, x, y, w, h, ic, oc, r, fe);
        } break;
        case Op::SaveSDFGlyph: {
            uint64_t const hash = buf.get<uint64_t>(pos);
            // Only mark the glyph cached once the real generation actually happened (mirrors SavePath).
            if (::nvgSaveSDFGlyph(R, hash) != -1)
                confirmSDFGlyphSaved(hash);
        } break;
        case Op::FillSDFGlyph: {
            uint64_t const hash = buf.get<uint64_t>(pos);
            NVGcolor col = buf.get<NVGcolor>(pos);
            ::nvgFillSDFGlyph(R, hash, col);
        } break;
        case Op::FillSDFGlyphRun: {
            NVGcolor const col = buf.get<NVGcolor>(pos);
            int const count = buf.get<int>(pos);
            uint32_t hbytes = 0; char const* hp = buf.getBytes(pos, hbytes);
            uint32_t xbytes = 0; char const* xp = buf.getBytes(pos, xbytes);
            if (count > 0) {
                // Copy into aligned, reusable scratch (buffer bytes are unaligned).
                sdfRunHashes.resize(static_cast<size_t>(count));
                sdfRunXforms.resize(static_cast<size_t>(count) * 6);
                std::memcpy(sdfRunHashes.data(), hp, hbytes);
                std::memcpy(sdfRunXforms.data(), xp, xbytes);
                ::nvgFillSDFGlyphRun(R, sdfRunHashes.data(), sdfRunXforms.data(), count, col);
            }
        } break;
        }
    }
}

bool Context::performRender()
{
    // In synchronous mode frames are already flushed at nvgEndFrame.
    if (synchronous)
        return false;

    // Grab the frame AND its committed resource ops under the same lock, so the
    // resources applied are exactly those of published frames up to this one --
    // never a delete from a frame still being recorded (that would flicker).
    CommandBuffer* buf = nullptr;
    std::vector<ResourceOp> ops;
    {
        std::lock_guard<std::mutex> lock(mutex);
        ops.swap(committedResources);
        buf = pending;
        pending = nullptr;
    }

    // Apply resources first, so every image the frame references already exists.
    if (!ops.empty())
        applyResources(ops);

    if (!buf)
        return false;

    replayBuffer(*buf);

    {
        std::lock_guard<std::mutex> lock(mutex);
        freeList.push_back(buf);
    }
    return true;
}

// ---------------------------------------------------------------------------
// Lifetime.
// ---------------------------------------------------------------------------
NVGcontext* create(NVGcontext* underlyingCtx, bool synchronous, int poolSize, size_t initialBufferBytes)
{
    if (poolSize < 3)
        poolSize = 3;   // minimum that guarantees no producer blocking/allocation

    auto* x = new Context();
    x->real = underlyingCtx;
    x->synchronous = synchronous;
    x->ownedBuffers.reserve(poolSize);
    x->freeList.reserve(poolSize);

    for (int i = 0; i < poolSize; ++i) {
        auto* b = new CommandBuffer(initialBufferBytes);
        x->ownedBuffers.push_back(b);
        x->freeList.push_back(b);
    }

    // Take one buffer to record the first frame into.
    x->record = x->freeList.back();
    x->freeList.pop_back();

    return reinterpret_cast<NVGcontext*>(x);
}

void destroy(NVGcontext* handle)
{
    if (!handle)
        return;
    auto* x = reinterpret_cast<Context*>(handle);
    for (auto& [framebuffer, info] : x->framebuffers) {
        if (info.real)
            nvgDeleteFramebuffer(info.real);
        delete reinterpret_cast<uint64_t*>(framebuffer);
    }
    for (auto* b : x->ownedBuffers)
        delete b;
    delete x;
}

} // namespace nanovg
