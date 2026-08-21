//
// nanovg_async.h
//
// A thread-decoupling command-buffer layer around NanoVG.
//
// The idea: the message/UI thread records NanoVG drawing calls into a tightly
// packed byte buffer instead of touching the GPU context directly. When it
// calls nvgEndFrame() the recorded frame is published (a cheap pointer swap
// under a very short lock) to the render/GPU thread, which replays it against
// the real NVGcontext from performRender().
//
// Usage
// -----
//   // ---- setup, on the render thread (owns the GL/Metal context) ----
//   NVGcontext* realCtx = nvgCreateContext(...);      // backend specific
//   NVGcontext* nvg     = nanovg::create(realCtx);    // <- the handle you draw with
//   nanovg::nvgCreateFontMem(nvg, ...);               // resources: forwarded to realCtx
//
//   // ---- message thread: record a frame ----
//   nanovg::nvgBeginFrame(nvg, w, h, dpr);
//   nanovg::nvgFillColor(nvg, nanovg::nvgRGBA(40,40,40,255));
//   nanovg::nvgFillRect(nvg, 0, 0, 40, 22);
//   nanovg::nvgEndFrame(nvg);                          // publishes the frame
//
//   // ---- render thread: draw the freshest published frame ----
//   nanovg::bindFramebuffer(nvg, fbo);                  // records backend FBO bind
//   nanovg::performRender(nvg);                        // replays onto realCtx
//
//   // ---- optional: cache repeated producer-side drawing command streams ----
//   nanovg::CommandBuffer cached;
//   cached.clear();
//   {
//       nanovg::ScopedCommandRecorder recorder(nvg, cached);
//       drawExpensiveStaticObject(nvg);
//   }
//   nanovg::replay(nvg, cached);                        // appends into this frame
//
//   // ---- teardown ----
//   nanovg::destroy(nvg);
//   nvgDeleteContext(realCtx);
//
// Rules
// -----
//  * All drawing/state calls become nanovg::nvgX(nvg, ...). Because the handle
//    is disguised as an NVGcontext*, your existing variables and function
//    signatures (void render(NVGcontext*)) do not have to change.
//  * The NanoVG backend helpers that are preprocessor macros cannot be
//    namespaced. Use nanovg::createFramebuffer/bindFramebuffer/viewport/clear
//    when those operations must be recorded, and call remaining backend macros
//    on nanovg::underlying(nvg) from the render thread.
//  * Recording is single-producer: only one thread may record + call nvgEndFrame.
//    performRender() is single-consumer: only one thread may call it. (SPSC.)
//  * If a frame is published while a previous one is still waiting to be drawn,
//    the older one is coalesced away (the render thread always draws the freshest
//    frame). If you cannot tolerate dropped frames, gate production with
//    nanovg::hasPendingFrame(nvg).
//  * Font resources and text queries run synchronously against the real context.
//    Image and framebuffer resources return virtual ids/handles and are resolved
//    during replay on the render thread.
//  * Render callbacks run during replay with the real NanoVG context. Use the
//    raw byte overload for POD data, or the owned-payload overload for C++ data
//    that needs construction/destruction if a frame is coalesced away.
//  * CommandBuffer caching is producer-side only. Cache regular drawing/state/text
//    emission; keep frame boundaries, framebuffer/resource lifecycle, and persistent
//    render-thread cache creation (e.g. nvgSavePath) outside cached replay regions.
//

#ifndef NANOVG_ASYNC_H
#define NANOVG_ASYNC_H

#include <cstdint>
#include <cstring>
#include <cstddef>
#include <cmath>
#include <atomic>
#include <mutex>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <type_traits>
#include <utility>

#include "nanovg.h"

namespace nanovg {

// ---------------------------------------------------------------------------
// Opcodes for every deferred (recorded) command.
// ---------------------------------------------------------------------------
enum class Op : uint8_t {
    // frame
    BeginFrame,
    EndFrame,
    // backend framebuffer commands
    CreateFramebuffer,
    DeleteFramebuffer,
    BindFramebuffer,
    BindMainFramebuffer,
    Viewport,
    Clear,
    RenderCallback,
    RenderOwnedCallback,
    // composite
    GlobalCompositeOperation,
    GlobalCompositeBlendFunc,
    GlobalCompositeBlendFuncSeparate,
    // state
    Save,
    Restore,
    Reset,
    // style
    ShapeAntiAlias,
    StrokeColor,
    StrokePaint,
    FillColor,
    FillPaint,
    MiterLimit,
    StrokeWidth,
    LineStyle,
    DashLength,
    DashPhaseOffset,
    LineCap,
    LineJoin,
    GlobalAlpha,
    // transforms
    ResetTransform,
    Transform,
    Translate,
    Rotate,
    SkewX,
    SkewY,
    Scale,
    TransformQuantize,
    // scissor
    GlobalScissor,
    Scissor,
    RoundedScissor,
    IntersectScissor,
    IntersectRoundedScissor,
    ResetScissor,
    // paths
    BeginPath,
    MoveTo,
    LineTo,
    BezierTo,
    QuadTo,
    ArcTo,
    ClosePath,
    PathWinding,
    Arc,
    Rect,
    RoundedRect,
    RoundedRectVarying,
    Ellipse,
    Circle,
    Fill,
    Stroke,
    // cached paths
    SavePath,
    StrokeCachedPath,
    FillCachedPath,
    DeletePath,
    // image resources
    CreateImageARGB,
    CreateImageARGBSRGB,
    CreateImageAlpha,
    UpdateImage,
    DeleteImage,
    // text
    Text,
    TextBox,
    FontSize,
    TextLetterSpacing,
    TextLineHeight,
    TextAlign,
    FontFaceId,
    FontFace,
    AtlasTextThreshold,
    // plugdata direct draws
    FillRect,
    StrokeRect,
    DrawRoundedRect,
    DrawObjectWithFlag,
    FillRoundedRect,
    SmoothGlow,
    // SDF glyph cache (generated on the consumer, mirroring SavePath / FillCachedPath)
    SaveSDFGlyph,
    FillSDFGlyph,
};

using RenderCallback = void (*)(NVGcontext* realContext, void const* data, uint32_t dataSize);
using OwnedRenderCallback = void (*)(NVGcontext* realContext, void* data);
using OwnedRenderPayloadDestroy = void (*)(void* data);

// ---------------------------------------------------------------------------
// A recorded command stream: a tightly packed, reusable byte buffer. Most
// instances hold a full frame; producer-side caches can hold partial draw streams.
//
// Arguments are appended by value with memcpy (so unaligned access is never a
// problem). reset() keeps the underlying storage, so after a few warm-up frames
// no further allocation happens while recording.
// ---------------------------------------------------------------------------
class CommandBuffer {
    struct OwnedRenderPayload {
        std::atomic<uint32_t> refCount { 1 };
        OwnedRenderCallback callback = nullptr;
        void* data = nullptr;
        OwnedRenderPayloadDestroy destroy = nullptr;

        void incReferenceCount() noexcept
        {
            refCount.fetch_add(1, std::memory_order_relaxed);
        }

        void decReferenceCount() noexcept
        {
            if (refCount.fetch_sub(1, std::memory_order_acq_rel) == 1) {
                if (destroy != nullptr)
                    destroy(data);

                delete this;
            }
        }
    };

public:
    explicit CommandBuffer(size_t initialBytes = 64)
    {
        if (initialBytes < 64)
            initialBytes = 64;
        storage_.resize(initialBytes);
    }

    ~CommandBuffer()
    {
        releaseOwnedRenderPayloads();
    }

    // --- writing (producer thread) ---
    inline void reset() noexcept
    {
        releaseOwnedRenderPayloads();
        size_ = 0;
    }

    inline void clear() noexcept { reset(); }
    inline size_t size() const noexcept { return size_; }
    inline bool empty() const noexcept { return size_ == 0; }

    template <typename T>
    inline void put(T const& value)
    {
        static_assert(std::is_trivially_copyable<T>::value, "recorded arg must be trivially copyable");
        ensure(sizeof(T));
        std::memcpy(storage_.data() + size_, &value, sizeof(T));
        size_ += sizeof(T);
    }

    inline void putOp(Op op) { put(static_cast<uint8_t>(op)); }

    inline void append(CommandBuffer const& other)
    {
        if (other.size_ == 0)
            return;

        auto renderPayloads = other.ownedRenderPayloads_;
        for (auto* payload : renderPayloads)
            if (payload != nullptr)
                payload->incReferenceCount();

        size_t const oldSize = size_;
        ensure(other.size_);
        if (&other == this)
            std::memcpy(storage_.data() + oldSize, storage_.data(), oldSize);
        else
            std::memcpy(storage_.data() + oldSize, other.storage_.data(), other.size_);
        size_ = oldSize + other.size_;

        ownedRenderPayloads_.insert(ownedRenderPayloads_.end(), renderPayloads.begin(), renderPayloads.end());
    }

    // Copies n raw bytes inline (length-prefixed) - used for text/font strings.
    inline void putBytes(void const* src, uint32_t n)
    {
        put(n);
        if (n) {
            ensure(n);
            std::memcpy(storage_.data() + size_, src, n);
            size_ += n;
        }
    }

    // --- reading (consumer thread) ---
    template <typename T>
    inline T get(size_t& pos) const
    {
        T value;
        std::memcpy(&value, storage_.data() + pos, sizeof(T));
        pos += sizeof(T);
        return value;
    }

    inline Op getOp(size_t& pos) const { return static_cast<Op>(get<uint8_t>(pos)); }

    // Returns a pointer into the buffer valid for the duration of the replay.
    inline char const* getBytes(size_t& pos, uint32_t& n) const
    {
        n = get<uint32_t>(pos);
        char const* p = reinterpret_cast<char const*>(storage_.data() + pos);
        pos += n;
        return p;
    }

    inline void putOwnedRenderCallback(OwnedRenderCallback callback, void* data, OwnedRenderPayloadDestroy destroy)
    {
        auto* payload = new OwnedRenderPayload();
        payload->callback = callback;
        payload->data = data;
        payload->destroy = destroy;
        ownedRenderPayloads_.push_back(payload);
        put(reinterpret_cast<uintptr_t>(payload));
    }

    inline void callOwnedRenderCallback(NVGcontext* realContext, size_t& pos) const
    {
        auto* payload = reinterpret_cast<OwnedRenderPayload*>(get<uintptr_t>(pos));

        if (payload != nullptr && payload->callback != nullptr)
            payload->callback(realContext, payload->data);
    }

private:
    void releaseOwnedRenderPayloads() noexcept
    {
        for (auto* payload : ownedRenderPayloads_)
            if (payload != nullptr)
                payload->decReferenceCount();

        ownedRenderPayloads_.clear();
    }

    inline void ensure(size_t extra)
    {
        size_t const need = size_ + extra;
        if (need > storage_.size())
            grow(need);
    }

    // Only ever taken during warm-up; steady-state frames stay within capacity.
    void grow(size_t need)
    {
        size_t cap = storage_.size() ? storage_.size() : 64;
        while (cap < need)
            cap *= 2;
        storage_.resize(cap);
    }

    std::vector<uint8_t> storage_;
    std::vector<OwnedRenderPayload*> ownedRenderPayloads_;
    size_t size_ = 0;
};

// ---------------------------------------------------------------------------
// The async context. Disguised behind an NVGcontext* handle so that call sites
// and signatures that use NVGcontext* keep working unchanged.
// ---------------------------------------------------------------------------
struct Context {
    NVGcontext* real = nullptr;       // real GPU context; touched only by the consumer
    float devicePxRatio = 1.0f;       // cached from nvgBeginFrame (for nvgDoubleStroke)

    // The surface's persistent main/damage framebuffer. Owned entirely by the
    // consumer (GL) thread: it sets this each frame before performRender, and the
    // recorded `Op::BindMainFramebuffer` binds it. Touched only by the consumer, so
    // no lock. Lets the message thread record "bind the main target" without
    // knowing (or managing) the real framebuffer.
    NVGframebuffer* mainTarget = nullptr;

    // When true, nvgEndFrame replays the frame immediately on the calling thread
    // instead of publishing it for a separate render thread. Used to run the
    // whole system single-threaded (record + flush on one thread).
    bool synchronous = false;

    // Producer-thread-only: the buffer currently being recorded into.
    CommandBuffer* record = nullptr;

    // Shared handoff, guarded by mutex_. Kept intentionally tiny.
    std::mutex mutex;
    std::vector<CommandBuffer*> ownedBuffers;   // owns every allocation
    std::vector<CommandBuffer*> freeList;        // buffers available for recording
    CommandBuffer* pending = nullptr;            // freshest published frame, or null

    struct ImageInfo {
        int realId = 0;
        int width = 0;
        int height = 0;
        int bytesPerPixel = 4;
    };
    struct FramebufferInfo {
        NVGframebuffer* real = nullptr;
        int virtualImageId = 0;
        int width = 0;
        int height = 0;
        int imageFlags = 0;
    };
    mutable std::mutex resourceMutex;
    std::unordered_map<int, ImageInfo> images;
    std::unordered_map<void*, FramebufferInfo> framebuffers;
    std::unordered_set<uint32_t> paths;
    // Consumer-owned membership for the render-thread SDF glyph cache (same rationale as `paths`:
    // set only once the real nvgSaveSDFGlyph has actually replayed, so it survives frame coalescing).
    std::unordered_set<uint64_t> sdfGlyphs;
    int nextImageId = 0x40000000;
    uint64_t nextFramebufferId = 1;
    uint32_t nextPathId = 1;

    // -----------------------------------------------------------------------
    // Resource queue.
    //
    // Image create/update/delete must NOT ride in the per-frame command buffer:
    // frames are coalesced (only the freshest published frame is replayed), so a
    // frame that created an image can be dropped, leaving its realId at 0 and a
    // virtual id (>= 0x40000000) leaking into the GL backend -- which indexes
    // gl->textures[id-1] and reads wildly out of bounds -> crash. Instead these
    // ops go into this queue, which the consumer drains IN FULL before every
    // replay, so a resource op is never lost to coalescing.
    // -----------------------------------------------------------------------
    struct ResourceOp {
        enum class Kind : uint8_t { CreateARGB, CreateARGBSRGB, CreateAlpha, Update, Delete };
        Kind kind;
        int image = 0;
        int width = 0;
        int height = 0;
        int flags = 0;
        std::vector<unsigned char> data;   // pixel payload (empty == none)
    };
    // Resource ops are staged per-frame and committed to the consumer ATOMICALLY
    // with the frame that uses them. Draining them independently of the published
    // frame would let a delete recorded by a not-yet-published frame run ahead of
    // the older frame still referencing that resource (resolveImageId -> 0 -> the
    // image draws nothing for one frame == flicker, seen on zoom's updateFramebuffers).
    std::vector<ResourceOp> recordingResources;   // producer-only: current frame's ops
    std::vector<ResourceOp> committedResources;    // guarded by `mutex`: published, awaiting the consumer

    void enqueueResource(ResourceOp&& op);            // producer: stage into the current frame
    void applyResources(std::vector<ResourceOp>& ops); // consumer: create/update/delete on the real ctx

    int allocateImageId(int width, int height, int bytesPerPixel);
    void* allocateFramebuffer(int width, int height, int imageFlags);
    int framebufferImage(void* framebuffer) const;
    void* underlyingFramebuffer(void* framebuffer) const;   // real FBO for a virtual handle (consumer side)

    uint32_t allocatePathId(uint32_t pathId);   // producer: assign an id (does not mark cached)
    void confirmPathSaved(uint32_t pathId);      // consumer: real nvgSavePath succeeded
    void confirmPathDeleted(uint32_t pathId);    // consumer: real nvgDeletePath ran
    bool checkPathId(uint32_t pathId);           // producer: is it confirmed-cached?

    void confirmSDFGlyphSaved(uint64_t hash);    // consumer: real nvgSaveSDFGlyph succeeded
    bool checkSDFGlyph(uint64_t hash);           // producer: is this glyph confirmed-cached?

    int resolveImageId(int image) const;
    NVGpaint resolvePaint(NVGpaint paint) const;

    // Publish the current record buffer and pick up a fresh one to record into.
    void publish();

    // Replay a command buffer onto `real` (consumer thread). Defined in the .cpp.
    void replayBuffer(CommandBuffer const& buf);

    // Replay the freshest published frame onto `real`. Returns true if it drew.
    bool performRender();
};

// ---------------------------------------------------------------------------
// Lifetime / control.
// ---------------------------------------------------------------------------

// Wrap an existing (backend-created) NVGcontext. `poolSize` buffers are
// pre-allocated (minimum 3 to guarantee the producer never allocates or blocks);
// `initialBufferBytes` is the starting capacity of each buffer.
//
// When `synchronous` is true the layer runs on a single thread: nvgEndFrame
// replays the frame immediately on the calling thread (performRender becomes a
// no-op). This is the drop-in, single-threaded configuration.
NVGcontext* create(NVGcontext* underlyingCtx, bool synchronous = false, int poolSize = 3, size_t initialBufferBytes = 1u << 18);

// Destroy the wrapper. Does NOT delete the underlying NVGcontext.
void destroy(NVGcontext* handle);

// Toggle synchronous (same-thread flush) mode. Only change this when no frame
// is mid-recording and no performRender is running.
inline void setSynchronous(NVGcontext* handle, bool synchronous)
{
    reinterpret_cast<Context*>(handle)->synchronous = synchronous;
}

// The real GPU context, for backend macros (framebuffers, blit, viewport, ...).
inline NVGcontext* underlying(NVGcontext* handle)
{
    return reinterpret_cast<Context*>(handle)->real;
}

// Replay the freshest published frame. Call from the render/GPU thread.
inline bool performRender(NVGcontext* handle)
{
    return reinterpret_cast<Context*>(handle)->performRender();
}

// True if a published-but-not-yet-drawn frame is waiting.
bool hasPendingFrame(NVGcontext* handle);

void* createFramebuffer(NVGcontext* c, int width, int height, int imageFlags);
void deleteFramebuffer(NVGcontext* c, void* framebuffer);
void bindFramebuffer(NVGcontext* c, void* framebuffer);
void viewport(NVGcontext* c, int x, int y, int width, int height);
void clear(NVGcontext* c);
int framebufferImage(NVGcontext* c, void* framebuffer);

// Consumer (GL) thread: designate the real framebuffer that recorded
// `bindMainFramebuffer()` ops resolve to. Call before performRender. The main
// framebuffer is owned entirely by the GL thread; the message thread never sees
// the real handle.
void setMainFramebuffer(NVGcontext* c, NVGframebuffer* realFramebuffer);
// Producer (message) thread: record "bind the main render target" at this point
// in the stream. Resolves to whatever the consumer last passed to
// setMainFramebuffer.
void bindMainFramebuffer(NVGcontext* c);

// The real backend framebuffer behind a virtual handle, or nullptr if it has
// not been created yet. Call from the render thread (after performRender) to
// blit / read back the persistent framebuffer.
void* underlyingFramebuffer(NVGcontext* c, void* framebuffer);

// ---------------------------------------------------------------------------
// Internal helpers used by the inline recording wrappers below.
// ---------------------------------------------------------------------------
namespace detail {
    inline Context* ctx(NVGcontext* c) { return reinterpret_cast<Context*>(c); }
    inline CommandBuffer& rec(NVGcontext* c) { return *reinterpret_cast<Context*>(c)->record; }
    inline NVGcontext* real(NVGcontext* c) { return reinterpret_cast<Context*>(c)->real; }
}

// Temporarily record async drawing calls into `target` instead of the current
// frame. This is producer-thread-only and assumes scopes are not nested.
class ScopedCommandRecorder {
public:
    ScopedCommandRecorder(NVGcontext* c, CommandBuffer& target)
        : context_(detail::ctx(c)), previous_(context_->record)
    {
        context_->record = &target;
    }

    ~ScopedCommandRecorder()
    {
        context_->record = previous_;
    }

    ScopedCommandRecorder(ScopedCommandRecorder const&) = delete;
    ScopedCommandRecorder& operator=(ScopedCommandRecorder const&) = delete;

private:
    Context* context_;
    CommandBuffer* previous_;
};

// Producer-thread replay: append previously recorded async commands into the
// frame currently being recorded. Commands must come from the same async context
// because virtual resource ids are context-owned. The render thread still
// performs real replay.
inline void replay(NVGcontext* c, CommandBuffer const& commands)
{
    detail::rec(c).append(commands);
}

// Queue custom render-thread work. `data` is copied into the command stream and
// is valid only for the duration of the callback during replay. The callback
// receives the real backend NVGcontext, so call raw ::nvg* functions inside it.
inline void nvgRenderCallback(NVGcontext* c, RenderCallback callback, void const* data = nullptr, uint32_t dataSize = 0)
{
    if (callback == nullptr)
        return;

    auto& b = detail::rec(c);
    b.putOp(Op::RenderCallback);
    b.put(callback);
    b.putBytes(data, data != nullptr ? dataSize : 0u);
}

namespace detail {
    template <typename Payload>
    struct TypedRenderCallbackPayload {
        using Callback = void (*)(NVGcontext*, Payload&);

        Callback callback = nullptr;
        Payload payload;
    };

    template <typename Payload>
    inline void invokeTypedRenderCallback(NVGcontext* realContext, void* data)
    {
        auto& payload = *static_cast<TypedRenderCallbackPayload<Payload>*>(data);

        if (payload.callback != nullptr)
            payload.callback(realContext, payload.payload);
    }

    template <typename Payload>
    inline void destroyTypedRenderCallbackPayload(void* data)
    {
        delete static_cast<TypedRenderCallbackPayload<Payload>*>(data);
    }
}

// Queue render-thread work with an owned C++ payload. The payload is moved into
// the command buffer, shared when command buffers are appended, and destroyed if
// the command is replayed, coalesced away, cancelled, or the buffer is destroyed.
template <typename Payload>
inline void nvgRenderCallback(NVGcontext* c, void (*callback)(NVGcontext*, Payload&), Payload payload)
{
    if (callback == nullptr)
        return;

    using StoredPayload = std::decay_t<Payload>;
    using CallbackPayload = detail::TypedRenderCallbackPayload<StoredPayload>;

    auto* callbackPayload = new CallbackPayload { callback, std::move(payload) };

    auto& b = detail::rec(c);
    b.putOp(Op::RenderOwnedCallback);
    b.putOwnedRenderCallback(detail::invokeTypedRenderCallback<StoredPayload>,
        callbackPayload,
        detail::destroyTypedRenderCallbackPayload<StoredPayload>);
}

// ===========================================================================
// DEFERRED commands - recorded, replayed later on the render thread.
// ===========================================================================

// --- frame ---
inline void nvgBeginFrame(NVGcontext* c, float windowWidth, float windowHeight, float devicePixelRatio)
{
    auto* x = detail::ctx(c);
    x->devicePxRatio = devicePixelRatio;
    auto& b = *x->record;
    b.putOp(Op::BeginFrame);
    b.put(windowWidth); b.put(windowHeight); b.put(devicePixelRatio);
}

inline void nvgEndFrame(NVGcontext* c)
{
    auto* x = detail::ctx(c);
    x->record->putOp(Op::EndFrame);
    if (x->synchronous) {
        // Single-threaded: apply this frame's resources, then replay immediately on
        // this thread and reuse the buffer. No hand-off, no pending slot.
        if (!x->recordingResources.empty())
            x->applyResources(x->recordingResources);
        x->recordingResources.clear();
        x->replayBuffer(*x->record);
        x->record->reset();
    } else {
        x->publish();
    }
}

inline void nvgEndFrameWithoutPublishing(NVGcontext* c)
{
    detail::ctx(c)->record->putOp(Op::EndFrame);
}

// Abort the frame being recorded: discard it, publish nothing.
inline void nvgCancelFrame(NVGcontext* c)
{
    detail::rec(c).reset();
}

// --- composite ---
inline void nvgGlobalCompositeOperation(NVGcontext* c, int op)
{
    auto& b = detail::rec(c); b.putOp(Op::GlobalCompositeOperation); b.put(op);
}
inline void nvgGlobalCompositeBlendFunc(NVGcontext* c, enum NVGblendFactor sfactor, enum NVGblendFactor dfactor)
{
    auto& b = detail::rec(c); b.putOp(Op::GlobalCompositeBlendFunc); b.put((int)sfactor); b.put((int)dfactor);
}
inline void nvgGlobalCompositeBlendFuncSeparate(NVGcontext* c, enum NVGblendFactor srcRGB, enum NVGblendFactor dstRGB, enum NVGblendFactor srcAlpha, enum NVGblendFactor dstAlpha)
{
    auto& b = detail::rec(c); b.putOp(Op::GlobalCompositeBlendFuncSeparate);
    b.put((int)srcRGB); b.put((int)dstRGB); b.put((int)srcAlpha); b.put((int)dstAlpha);
}

// --- state ---
inline void nvgSave(NVGcontext* c)    { auto* x = detail::ctx(c); x->record->putOp(Op::Save); }
inline void nvgRestore(NVGcontext* c) { auto* x = detail::ctx(c); x->record->putOp(Op::Restore);}
inline void nvgReset(NVGcontext* c)   { auto* x = detail::ctx(c); x->record->putOp(Op::Reset); }

// --- style ---
inline void nvgShapeAntiAlias(NVGcontext* c, int enabled) { auto& b = detail::rec(c); b.putOp(Op::ShapeAntiAlias); b.put(enabled); }
inline void nvgStrokeColor(NVGcontext* c, NVGcolor color) { auto& b = detail::rec(c); b.putOp(Op::StrokeColor); b.put(color); }
inline void nvgStrokePaint(NVGcontext* c, NVGpaint paint) { auto& b = detail::rec(c); b.putOp(Op::StrokePaint); b.put(paint); }
inline void nvgFillColor(NVGcontext* c, NVGcolor color)   { auto& b = detail::rec(c); b.putOp(Op::FillColor); b.put(color); }
inline void nvgFillPaint(NVGcontext* c, NVGpaint paint)   { auto& b = detail::rec(c); b.putOp(Op::FillPaint); b.put(paint); }
inline void nvgMiterLimit(NVGcontext* c, float limit)     { auto& b = detail::rec(c); b.putOp(Op::MiterLimit); b.put(limit); }
inline void nvgStrokeWidth(NVGcontext* c, float size)     { auto& b = detail::rec(c); b.putOp(Op::StrokeWidth); b.put(size); }
inline void nvgLineStyle(NVGcontext* c, int lineStyle)    { auto& b = detail::rec(c); b.putOp(Op::LineStyle); b.put(lineStyle); }
inline void nvgDashLength(NVGcontext* c, float length)    { auto& b = detail::rec(c); b.putOp(Op::DashLength); b.put(length); }
inline void nvgDashPhaseOffset(NVGcontext* c, float offset){ auto& b = detail::rec(c); b.putOp(Op::DashPhaseOffset); b.put(offset); }
inline void nvgLineCap(NVGcontext* c, int cap)            { auto& b = detail::rec(c); b.putOp(Op::LineCap); b.put(cap); }
inline void nvgLineJoin(NVGcontext* c, int join)          { auto& b = detail::rec(c); b.putOp(Op::LineJoin); b.put(join); }
inline void nvgGlobalAlpha(NVGcontext* c, float alpha)    { auto& b = detail::rec(c); b.putOp(Op::GlobalAlpha); b.put(alpha); }

// --- transforms ---
inline void nvgResetTransform(NVGcontext* c)
{
    auto* x = detail::ctx(c); x->record->putOp(Op::ResetTransform);
}
inline void nvgTransform(NVGcontext* c, float a, float b_, float cc, float d, float e, float f)
{
    auto* x = detail::ctx(c); auto& b = *x->record; b.putOp(Op::Transform);
    b.put(a); b.put(b_); b.put(cc); b.put(d); b.put(e); b.put(f);
}
inline void nvgTranslate(NVGcontext* c, float x, float y)
{
    auto* X = detail::ctx(c); auto& b = *X->record; b.putOp(Op::Translate); b.put(x); b.put(y);
}
inline void nvgRotate(NVGcontext* c, float angle)
{
    auto* x = detail::ctx(c); auto& b = *x->record; b.putOp(Op::Rotate); b.put(angle);
}
inline void nvgSkewX(NVGcontext* c, float angle)
{
    auto* x = detail::ctx(c); auto& b = *x->record; b.putOp(Op::SkewX); b.put(angle);
}
inline void nvgSkewY(NVGcontext* c, float angle)
{
    auto* x = detail::ctx(c); auto& b = *x->record; b.putOp(Op::SkewY); b.put(angle);
}
inline void nvgScale(NVGcontext* c, float x, float y)
{
    auto* X = detail::ctx(c); auto& b = *X->record; b.putOp(Op::Scale); b.put(x); b.put(y);
}
inline void nvgTransformQuantize(NVGcontext* c)
{
    auto* x = detail::ctx(c); x->record->putOp(Op::TransformQuantize);
}

// --- scissor ---
inline void nvgGlobalScissor(NVGcontext* c, int x, int y, int w, int h)
{
    auto& b = detail::rec(c); b.putOp(Op::GlobalScissor); b.put(x); b.put(y); b.put(w); b.put(h);
}
inline void nvgScissor(NVGcontext* c, float x, float y, float w, float h)
{
    auto* X = detail::ctx(c); auto& b = *X->record; b.putOp(Op::Scissor); b.put(x); b.put(y); b.put(w); b.put(h);
}
inline void nvgRoundedScissor(NVGcontext* c, float x, float y, float w, float h, float r)
{
    auto* X = detail::ctx(c); auto& b = *X->record; b.putOp(Op::RoundedScissor); b.put(x); b.put(y); b.put(w); b.put(h); b.put(r);
}
inline void nvgIntersectScissor(NVGcontext* c, float x, float y, float w, float h)
{
    auto& b = detail::rec(c); b.putOp(Op::IntersectScissor); b.put(x); b.put(y); b.put(w); b.put(h);
}
inline void nvgIntersectRoundedScissor(NVGcontext* c, float x, float y, float w, float h, float r)
{
    auto& b = detail::rec(c); b.putOp(Op::IntersectRoundedScissor); b.put(x); b.put(y); b.put(w); b.put(h); b.put(r);
}
inline void nvgResetScissor(NVGcontext* c) { detail::rec(c).putOp(Op::ResetScissor); }

// --- paths ---
inline void nvgBeginPath(NVGcontext* c) { detail::rec(c).putOp(Op::BeginPath); }
inline void nvgMoveTo(NVGcontext* c, float x, float y) { auto& b = detail::rec(c); b.putOp(Op::MoveTo); b.put(x); b.put(y); }
inline void nvgLineTo(NVGcontext* c, float x, float y) { auto& b = detail::rec(c); b.putOp(Op::LineTo); b.put(x); b.put(y); }
inline void nvgBezierTo(NVGcontext* c, float c1x, float c1y, float c2x, float c2y, float x, float y)
{
    auto& b = detail::rec(c); b.putOp(Op::BezierTo); b.put(c1x); b.put(c1y); b.put(c2x); b.put(c2y); b.put(x); b.put(y);
}
inline void nvgQuadTo(NVGcontext* c, float cx, float cy, float x, float y)
{
    auto& b = detail::rec(c); b.putOp(Op::QuadTo); b.put(cx); b.put(cy); b.put(x); b.put(y);
}
inline void nvgArcTo(NVGcontext* c, float x1, float y1, float x2, float y2, float radius)
{
    auto& b = detail::rec(c); b.putOp(Op::ArcTo); b.put(x1); b.put(y1); b.put(x2); b.put(y2); b.put(radius);
}
inline void nvgClosePath(NVGcontext* c) { detail::rec(c).putOp(Op::ClosePath); }
inline void nvgPathWinding(NVGcontext* c, enum NVGwinding dir) { auto& b = detail::rec(c); b.putOp(Op::PathWinding); b.put((int)dir); }
inline void nvgArc(NVGcontext* c, float cx, float cy, float r, float a0, float a1, int dir)
{
    auto& b = detail::rec(c); b.putOp(Op::Arc); b.put(cx); b.put(cy); b.put(r); b.put(a0); b.put(a1); b.put(dir);
}
inline void nvgRect(NVGcontext* c, float x, float y, float w, float h)
{
    auto& b = detail::rec(c); b.putOp(Op::Rect); b.put(x); b.put(y); b.put(w); b.put(h);
}
inline void nvgRoundedRect(NVGcontext* c, float x, float y, float w, float h, float r)
{
    auto& b = detail::rec(c); b.putOp(Op::RoundedRect); b.put(x); b.put(y); b.put(w); b.put(h); b.put(r);
}
inline void nvgRoundedRectVarying(NVGcontext* c, float x, float y, float w, float h, float radTopLeft, float radTopRight, float radBottomRight, float radBottomLeft)
{
    auto& b = detail::rec(c); b.putOp(Op::RoundedRectVarying);
    b.put(x); b.put(y); b.put(w); b.put(h);
    b.put(radTopLeft); b.put(radTopRight); b.put(radBottomRight); b.put(radBottomLeft);
}
inline void nvgEllipse(NVGcontext* c, float cx, float cy, float rx, float ry)
{
    auto& b = detail::rec(c); b.putOp(Op::Ellipse); b.put(cx); b.put(cy); b.put(rx); b.put(ry);
}
inline void nvgCircle(NVGcontext* c, float cx, float cy, float r)
{
    auto& b = detail::rec(c); b.putOp(Op::Circle); b.put(cx); b.put(cy); b.put(r);
}
inline void nvgFill(NVGcontext* c)   { detail::rec(c).putOp(Op::Fill); }
inline void nvgStroke(NVGcontext* c) { detail::rec(c).putOp(Op::Stroke); }

// --- cached paths ---
// nvgSavePath records the current (deferred) path into the cache at replay time.
// The id is supplied by the caller, so we can hand it straight back.
inline int32_t nvgSavePath(NVGcontext* c, uint32_t pathId)
{
    pathId = detail::ctx(c)->allocatePathId(pathId);
    auto& b = detail::rec(c); b.putOp(Op::SavePath); b.put(pathId); return (int32_t)pathId;
}
inline int nvgStrokeCachedPath(NVGcontext* c, uint32_t pathId) {
    auto& b = detail::rec(c); b.putOp(Op::StrokeCachedPath); b.put(pathId);
    return detail::ctx(c)->checkPathId(pathId);
}
inline int nvgFillCachedPath(NVGcontext* c, uint32_t pathId)   {
    auto& b = detail::rec(c); b.putOp(Op::FillCachedPath); b.put(pathId);
    return detail::ctx(c)->checkPathId(pathId);
}
inline void nvgDeletePath(NVGcontext* c, uint32_t pathId)      {
    // `paths` membership is dropped by the consumer when it actually replays the
    // DeletePath op (confirmPathDeleted), so it stays in step with the real cache.
    auto& b = detail::rec(c); b.putOp(Op::DeletePath); b.put(pathId);
}

// --- plugdata direct draws ---
inline void nvgFillRect(NVGcontext* c, float x1, float y1, float w, float h)
{
    auto& b = detail::rec(c); b.putOp(Op::FillRect); b.put(x1); b.put(y1); b.put(w); b.put(h);
}
inline void nvgStrokeRect(NVGcontext* c, float x1, float y1, float w, float h)
{
    auto& b = detail::rec(c); b.putOp(Op::StrokeRect); b.put(x1); b.put(y1); b.put(w); b.put(h);
}
inline void nvgDrawRoundedRect(NVGcontext* c, float x, float y, float w, float h, NVGcolor icol, NVGcolor ocol, float radius)
{
    auto& b = detail::rec(c); b.putOp(Op::DrawRoundedRect);
    b.put(x); b.put(y); b.put(w); b.put(h); b.put(icol); b.put(ocol); b.put(radius);
}
inline void nvgDrawObjectWithFlag(NVGcontext* c, float x, float y, float w, float h, NVGcolor icol, NVGcolor ocol, NVGcolor flagCol, float radius, enum ObjectFlagType flagType, bool flagOutline)
{
    auto& b = detail::rec(c); b.putOp(Op::DrawObjectWithFlag);
    b.put(x); b.put(y); b.put(w); b.put(h);
    b.put(icol); b.put(ocol); b.put(flagCol); b.put(radius);
    b.put((int)flagType); b.put((uint8_t)(flagOutline ? 1 : 0));
}
inline void nvgFillRoundedRect(NVGcontext* c, float x, float y, float w, float h, float radius)
{
    auto& b = detail::rec(c); b.putOp(Op::FillRoundedRect); b.put(x); b.put(y); b.put(w); b.put(h); b.put(radius);
}
inline void nvgSmoothGlow(NVGcontext* c, float x, float y, float w, float h, NVGcolor icol, NVGcolor ocol, float radius, float feather)
{
    auto& b = detail::rec(c); b.putOp(Op::SmoothGlow);
    b.put(x); b.put(y); b.put(w); b.put(h); b.put(icol); b.put(ocol); b.put(radius); b.put(feather);
}
// --- SDF glyph cache (mirrors the cached-path API) ---
// Producer-side query: has the consumer confirmed a tile for this glyph? (No op recorded.)
inline bool nvgSDFGlyphCached(NVGcontext* c, uint64_t hash) { return detail::ctx(c)->checkSDFGlyph(hash); }
// Record the current path as a glyph to be turned into an SDF tile on the consumer.
inline void nvgSaveSDFGlyph(NVGcontext* c, uint64_t hash)
{
    auto& b = detail::rec(c); b.putOp(Op::SaveSDFGlyph); b.put(hash);
}
// Draw the cached SDF tile for `hash` at the current transform.
inline void nvgFillSDFGlyph(NVGcontext* c, uint64_t hash, NVGcolor color)
{
    auto& b = detail::rec(c); b.putOp(Op::FillSDFGlyph); b.put(hash); b.put(color);
}

// nvgDoubleStroke both builds a paint AND mutates the state's line style. We can
// build the paint on the calling thread (it depends only on the arguments and
// the cached device pixel ratio) and record the state change so it lands in the
// right order on the render thread. Mirrors nvgDoubleStroke in nanovg.cpp.
inline NVGpaint nvgDoubleStroke(NVGcontext* c, NVGcolor icol, NVGcolor ocol, NVGcolor dashCol, float dashSize, bool isGradientStroke, bool showActivity, float activityOffset)
{
    auto* x = detail::ctx(c);
    NVGpaint p;
    std::memset(&p, 0, sizeof(p));
    p.xform[0] = 1.0f;
    p.xform[3] = 1.0f;
    p.radius = dashSize;
    p.feather = x->devicePxRatio < 2.0f ? 0.8f : 0.6f;
    p.innerColor = icol;
    p.outerColor = ocol;
    p.dashColor = dashCol;
    p.offset = activityOffset;
    if (showActivity)
        p.type = isGradientStroke ? PAINT_TYPE_DOUBLE_STROKE_GRAD_ACTIVITY : PAINT_TYPE_DOUBLE_STROKE_ACTIVITY;
    else
        p.type = isGradientStroke ? PAINT_TYPE_DOUBLE_STROKE_GRAD : PAINT_TYPE_DOUBLE_STROKE;
    p.connection_activity = showActivity;

    auto& b = *x->record; b.putOp(Op::LineStyle); b.put((int)NVG_DOUBLE_STROKE);
    return p;
}

// ===========================================================================
// PURE helpers - no dependency on live context state; forwarded immediately.
// ===========================================================================

// --- colors (no context) ---
inline NVGcolor nvgRGB(unsigned char r, unsigned char g, unsigned char b) { return ::nvgRGB(r, g, b); }
inline NVGcolor nvgRGBf(float r, float g, float b) { return ::nvgRGBf(r, g, b); }
inline NVGcolor nvgRGBA32(unsigned char r, unsigned char g, unsigned char b, unsigned char a) { return ::nvgRGBA32(r, g, b, a); }
inline NVGcolor nvgRGBA(unsigned char r, unsigned char g, unsigned char b, unsigned char a) { return ::nvgRGBA(r, g, b, a); }
inline NVGcolor nvgRGBAf(float r, float g, float b, float a) { return ::nvgRGBAf(r, g, b, a); }
inline NVGcolor nvgTransRGBA(NVGcolor c0, unsigned char a) { return ::nvgTransRGBA(c0, a); }
inline NVGcolor nvgTransRGBAf(NVGcolor c0, float a) { return ::nvgTransRGBAf(c0, a); }
inline NVGcolor nvgHSL(float h, float s, float l) { return ::nvgHSL(h, s, l); }
inline NVGcolor nvgHSLA(float h, float s, float l, unsigned char a) { return ::nvgHSLA(h, s, l, a); }

// --- gradient / pattern paint builders (pure w.r.t. live state) ---
inline NVGpaint nvgLinearGradient(NVGcontext* c, float sx, float sy, float ex, float ey, NVGcolor icol, NVGcolor ocol)
{ return ::nvgLinearGradient(detail::real(c), sx, sy, ex, ey, icol, ocol); }
inline NVGpaint nvgBoxGradient(NVGcontext* c, float x, float y, float w, float h, float r, float f, NVGcolor icol, NVGcolor ocol)
{ return ::nvgBoxGradient(detail::real(c), x, y, w, h, r, f, icol, ocol); }
inline NVGpaint nvgRadialGradient(NVGcontext* c, float cx, float cy, float inr, float outr, NVGcolor icol, NVGcolor ocol)
{ return ::nvgRadialGradient(detail::real(c), cx, cy, inr, outr, icol, ocol); }
inline NVGpaint nvgImagePattern(NVGcontext* c, float ox, float oy, float ex, float ey, float angle, int image, float alpha)
{ return ::nvgImagePattern(detail::real(c), ox, oy, ex, ey, angle, image, alpha); }
inline NVGpaint nvgImageAlphaPattern(NVGcontext* c, float ox, float oy, float ex, float ey, float angle, int image, NVGcolor iCol)
{ return ::nvgImageAlphaPattern(detail::real(c), ox, oy, ex, ey, angle, image, iCol); }
inline NVGpaint nvgDotPattern(NVGcontext* c, NVGcolor icol, NVGcolor ocol, float patternSize, float dotRadius, float feather)
{ return ::nvgDotPattern(detail::real(c), icol, ocol, patternSize, dotRadius, feather); }

// --- transform math (operate on float[], no context) ---
inline void nvgTransformIdentity(float* dst) { ::nvgTransformIdentity(dst); }
inline void nvgTransformTranslate(float* dst, float tx, float ty) { ::nvgTransformTranslate(dst, tx, ty); }
inline void nvgTransformScale(float* dst, float sx, float sy) { ::nvgTransformScale(dst, sx, sy); }
inline void nvgTransformRotate(float* dst, float a) { ::nvgTransformRotate(dst, a); }
inline void nvgTransformSkewX(float* dst, float a) { ::nvgTransformSkewX(dst, a); }
inline void nvgTransformSkewY(float* dst, float a) { ::nvgTransformSkewY(dst, a); }
inline void nvgTransformMultiply(float* dst, const float* src) { ::nvgTransformMultiply(dst, src); }
inline void nvgTransformPremultiply(float* dst, const float* src) { ::nvgTransformPremultiply(dst, src); }
inline int nvgTransformInverse(float* dst, const float* src) { return ::nvgTransformInverse(dst, src); }
inline void nvgTransformPoint(float* dstx, float* dsty, const float* xform, float srcx, float srcy) { ::nvgTransformPoint(dstx, dsty, xform, srcx, srcy); }
inline float nvgDegToRad(float deg) { return ::nvgDegToRad(deg); }
inline float nvgRadToDeg(float rad) { return ::nvgRadToDeg(rad); }

// ===========================================================================
// SYNCHRONOUS on the real context - resources & queries. These return values,
// so they run immediately against the underlying context. Only call them where
// that is safe (see the header notes at the top).
// ===========================================================================

// --- images ---
inline int nvgCreateImage(NVGcontext* c, char const* filename, int imageFlags) { return ::nvgCreateImage(detail::real(c), filename, imageFlags); }
inline int nvgCreateImageMem(NVGcontext* c, int imageFlags, unsigned char* data, int ndata) { return ::nvgCreateImageMem(detail::real(c), imageFlags, data, ndata); }

inline int nvgCreateImageARGB(NVGcontext* c, int w, int h, int imageFlags, const unsigned char* data)
{
    auto* x = detail::ctx(c);
    int const image = x->allocateImageId(w, h, 4);
    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::CreateARGB;
    op.image = image; op.width = w; op.height = h; op.flags = imageFlags;
    if (data) op.data.assign(data, data + static_cast<size_t>(w) * h * 4);
    x->enqueueResource(std::move(op));
    return image;
}
inline int nvgCreateImageARGB_sRGB(NVGcontext* c, int w, int h, int imageFlags, const unsigned char* data)
{
    auto* x = detail::ctx(c);
    int const image = x->allocateImageId(w, h, 4);
    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::CreateARGBSRGB;
    op.image = image; op.width = w; op.height = h; op.flags = imageFlags;
    if (data) op.data.assign(data, data + static_cast<size_t>(w) * h * 4);
    x->enqueueResource(std::move(op));
    return image;
}
inline int nvgCreateImageAlpha(NVGcontext* c, int w, int h, int imageFlags, const unsigned char* data)
{
    auto* x = detail::ctx(c);
    int const image = x->allocateImageId(w, h, 1);
    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::CreateAlpha;
    op.image = image; op.width = w; op.height = h; op.flags = imageFlags;
    if (data) op.data.assign(data, data + static_cast<size_t>(w) * h);
    x->enqueueResource(std::move(op));
    return image;
}
inline void nvgUpdateImage(NVGcontext* c, int image, const unsigned char* data)
{
    auto* x = detail::ctx(c);
    size_t byteCount = 0;
    {
        std::lock_guard<std::mutex> lock(x->resourceMutex);
        auto const iter = x->images.find(image);
        byteCount = iter == x->images.end() ? 0u : static_cast<size_t>(iter->second.width) * iter->second.height * iter->second.bytesPerPixel;
    }
    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::Update;
    op.image = image;
    if (data && byteCount) op.data.assign(data, data + byteCount);
    x->enqueueResource(std::move(op));
}
inline void nvgImageSize(NVGcontext* c, int image, int* w, int* h)
{
    auto* x = detail::ctx(c);
    {
        std::lock_guard<std::mutex> lock(x->resourceMutex);
        auto const iter = x->images.find(image);
        if (iter != x->images.end()) {
            if (w) *w = iter->second.width;
            if (h) *h = iter->second.height;
            return;
        }
    }

    ::nvgImageSize(detail::real(c), image, w, h);
}
inline void nvgDeleteImage(NVGcontext* c, int image)
{
    Context::ResourceOp op;
    op.kind = Context::ResourceOp::Kind::Delete;
    op.image = image;
    detail::ctx(c)->enqueueResource(std::move(op));
}
inline int nvgGetImageTextureId(NVGcontext* c, int handle) { return ::nvgGetImageTextureId(detail::real(c), detail::ctx(c)->resolveImageId(handle)); }
inline int nvgIsTexture(NVGcontext* c, int textureId) { return ::nvgIsTexture(detail::real(c), textureId); }

// --- fonts ---
inline int nvgCreateFont(NVGcontext* c, char const* name, char const* filename) { return ::nvgCreateFont(detail::real(c), name, filename); }
inline int nvgCreateFontAtIndex(NVGcontext* c, char const* name, char const* filename, const int fontIndex) { return ::nvgCreateFontAtIndex(detail::real(c), name, filename, fontIndex); }
inline int nvgCreateFontMem(NVGcontext* c, char const* name, unsigned char* data, int ndata, int freeData) { return ::nvgCreateFontMem(detail::real(c), name, data, ndata, freeData); }
inline int nvgCreateFontMemAtIndex(NVGcontext* c, char const* name, unsigned char* data, int ndata, int freeData, const int fontIndex) { return ::nvgCreateFontMemAtIndex(detail::real(c), name, data, ndata, freeData, fontIndex); }
inline int nvgFindFont(NVGcontext* c, char const* name) { return ::nvgFindFont(detail::real(c), name); }
inline int nvgAddFallbackFontId(NVGcontext* c, int baseFont, int fallbackFont) { return ::nvgAddFallbackFontId(detail::real(c), baseFont, fallbackFont); }
inline int nvgAddFallbackFont(NVGcontext* c, char const* baseFont, char const* fallbackFont) { return ::nvgAddFallbackFont(detail::real(c), baseFont, fallbackFont); }
inline void nvgResetFallbackFontsId(NVGcontext* c, int baseFont) { ::nvgResetFallbackFontsId(detail::real(c), baseFont); }
inline void nvgResetFallbackFonts(NVGcontext* c, char const* baseFont) { ::nvgResetFallbackFonts(detail::real(c), baseFont); }


inline float nvgCurrentPixelScale(NVGcontext* c) { return detail::ctx(c)->devicePxRatio; }
inline void setCurrentPixelScale(NVGcontext* c, float const devicePixelRatio) { detail::ctx(c)->devicePxRatio = devicePixelRatio; }

inline float nvgGetStrokeWidth(NVGcontext* c) { return ::nvgGetStrokeWidth(detail::real(c)); }

inline void nvgDebugDumpPathCache(NVGcontext* c) { ::nvgDebugDumpPathCache(detail::real(c)); }

} // namespace nanovg

#endif // NANOVG_ASYNC_H
