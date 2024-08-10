// Copyright (c) 2017 Ollix
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// ---
// Author: olliwang@ollix.com (Olli Wang)

#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdatomic.h>

#include "nanovg_mtl.h"
#import <Metal/Metal.h>
#if TARGET_OS_IPHONE
#import <UIKit/UIKit.h>
#else
#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>
#endif
#import <QuartzCore/CAMetalLayer.h>
#import <simd/simd.h>
#include <TargetConditionals.h>

#include "nanovg.h"

#if TARGET_OS_SIMULATOR
#  include "mnvg_bitcode/simulator.h"
#elif TARGET_OS_IOS
#  include "mnvg_bitcode/ios.h"
#elif TARGET_OS_OSX
#  include "mnvg_bitcode/macos.h"
#elif TARGET_OS_TV
#  include "mnvg_bitcode/tvos.h"
#else
#  define MNVG_INVALID_TARGET
#endif

typedef enum MNVGvertexInputIndex {
    MNVG_VERTEX_INPUT_INDEX_VERTICES = 0,
    MNVG_VERTEX_INPUT_INDEX_VIEW_SIZE = 1,
} MNVGvertexInputIndex;

typedef enum MNVGshaderType {
    MNVG_SHADER_FILLGRAD,
    MNVG_SHADER_FILLIMG,
    MNVG_SHADER_IMG,
    MNVG_SHADER_DOTS,
    MNVG_SHADER_FAST_ROUNDEDRECT,
    MNVG_SHADER_FILLCOLOR,
    MNVG_SHADER_DOUBLE_STROKE,
    MNVG_SHADER_SMOOTH_GLOW,
    MNVG_SHADER_DOUBLE_STROKE_GRAD,
    MNVG_SHADER_DOUBLE_STROKE_ACTIVITY,
    MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY
} MNVGshaderType;

enum MNVGcallType {
    MNVG_NONE = 0,
    MNVG_FILL,
    MNVG_CONVEXFILL,
    MNVG_STROKE,
    MNVG_TRIANGLES,
};

struct MNVGblend {
    MTLBlendFactor srcRGB;
    MTLBlendFactor dstRGB;
    MTLBlendFactor srcAlpha;
    MTLBlendFactor dstAlpha;
};
typedef struct MNVGblend MNVGblend;

struct MNVGcall {
    int type;
    int image;
    int pathOffset;
    int pathCount;
    int triangleOffset;
    int triangleCount;
    int indexOffset;
    int indexCount;
    int strokeOffset;
    int strokeCount;
    int uniformOffset;
    MNVGblend blendFunc;
};
typedef struct MNVGcall MNVGcall;

struct MNVGfragUniforms {
    matrix_float3x3 scissorMat;
    matrix_float3x3 paintMat;
    vector_float4 innerCol;
    vector_float4 outerCol;
    vector_float4 dashCol;
    vector_float2 scissorExt;
    vector_float2 scissorScale;
    vector_float2 extent;
    float radius;
    float feather;
    float strokeMult;
    float scissorRadius;
    float patternSize;
    float offset;
    float lineLength;
    int stateData;
};

typedef struct MNVGfragUniforms MNVGfragUniforms;


struct MNVGrenderData {
    atomic_int image;
    MNVGcall* calls;
    atomic_int ccalls;
    atomic_int ncalls;
    uint32_t* indexes;
    atomic_int cindexes;
    atomic_int nindexes;
    struct NVGvertex* verts;
    atomic_int cverts;
    atomic_int nverts;
    unsigned char* uniforms;
    atomic_int cuniforms;
    atomic_int nuniforms;
};
typedef struct MNVGrenderData MNVGrenderData;

@interface MNVGtexture : NSObject {
@public
    int id;
    int type;
    int flags;
    id<MTLTexture> tex;
    id<MTLSamplerState> sampler;
    bool valid;
}
@end

__attribute__((objc_direct_members))
@interface MNVGbuffers : NSObject

@property (nonatomic, strong) id<MTLCommandBuffer> commandBuffer;
@property (nonatomic, strong) id<MTLBuffer> viewSizeBuffer;
@property (nonatomic, strong) id<MTLTexture> stencilTexture;
@property (nonatomic, strong) id<MTLBuffer> indexBuffer;
@property (nonatomic, strong) id<MTLBuffer> vertBuffer;
@property (nonatomic, strong) id<MTLBuffer> uniformBuffer;
@property (nonatomic, assign) atomic_int isBusy;
@property (nonatomic, assign) MNVGrenderData* renderData;
@end

@interface MNVGcontext : NSObject

@property (nonatomic, strong) id<MTLCommandQueue> commandQueue;
@property (nonatomic, strong) CAMetalLayer* metalLayer;
@property (nonatomic, strong) id <MTLRenderCommandEncoder> renderEncoder;

@property (nonatomic, assign) int fragSize;
@property (nonatomic, assign) int indexSize;
@property (nonatomic, assign) int flags;
@property (nonatomic, assign) vector_uint2 viewPortSize;
@property (nonatomic, assign) MTLClearColor clearColor;
@property (nonatomic, assign) BOOL clearBufferOnFlush;
@property (nonatomic, assign) int lastUniformOffset;
@property (nonatomic, assign) int lastBoundTexture;
@property (nonatomic, weak)   id<MTLTexture> lastColorTexture;
// Textures
@property (nonatomic, strong) NSMutableArray<MNVGtexture*>* textures;
@property int textureId;

// Per frame buffers
@property (nonatomic, assign) MNVGbuffers* buffers;
@property (nonatomic, strong) NSMutableArray* cbuffers;
@property (nonatomic, assign) int maxBuffers;
@property (nonatomic, strong) dispatch_semaphore_t semaphore;

// Cached states.
@property (nonatomic, assign) MNVGblend* blendFunc;
@property (nonatomic, strong) id<MTLDepthStencilState> defaultStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState> fillShapeStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState>
fillAntiAliasStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState> fillStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState> strokeShapeStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState>
strokeAntiAliasStencilState;
@property (nonatomic, strong) id<MTLDepthStencilState> strokeClearStencilState;
@property (nonatomic, strong) id<MTLFunction> fragmentFunction;
@property (nonatomic, strong) id<MTLFunction> vertexFunction;
@property MTLPixelFormat piplelinePixelFormat;
@property (nonatomic, strong) id<MTLRenderPipelineState> pipelineState;
@property (nonatomic, strong) id<MTLRenderPipelineState>
stencilOnlyPipelineState;
@property (nonatomic, strong) id<MTLSamplerState> pseudoSampler;
@property (nonatomic, strong) id<MTLTexture> pseudoTexture;
@property (nonatomic, strong) MTLVertexDescriptor* vertexDescriptor;

- (MNVGtexture*)allocTexture;

- (void)convexFill:(MNVGcall*)call;

-(void)fill:(MNVGcall*)call;

- (MNVGtexture*)findTexture:(int)id;

- (void)renderCancel;

- (int)renderCreate;

- (void)renderDelete;

- (int)renderDeleteTexture:(int)image;

- (int)renderCreateTextureWithType:(int)type
                             width:(int)width
                            height:(int)height
                        imageFlags:(int)imageFlags
                              data:(const unsigned char*)data;

- (void)renderFillWithPaint:(NVGpaint*)paint
         compositeOperation:(NVGcompositeOperationState)compositeOperation
                    scissor:(NVGscissor*)scissor
                     fringe:(float)fringe
                     bounds:(const float*)bounds
                      paths:(const NVGpath*)paths
                     npaths:(int)npaths;

- (void)renderFlush;

- (int)renderGetTextureSizeForImage:(int)image
                              width:(int*)width
                             height:(int*)height;

- (void)renderStrokeWithPaint:(NVGpaint*)paint
           compositeOperation:(NVGcompositeOperationState)compositeOperation
                      scissor:(NVGscissor*)scissor
                       fringe:(float)fringe
                  strokeWidth:(float)strokeWidth
                    lineStyle:(int)lineStyle
                   lineLength:(float)lineLength
                        paths:(const NVGpath*)paths
                       npaths:(int)npaths;

- (void)renderTrianglesWithPaint:(NVGpaint*) paint
              compositeOperation:(NVGcompositeOperationState)compositeOperation
                         scissor:(NVGscissor*)scissor
                           verts:(const NVGvertex*)verts
                          nverts:(int)nverts
                          fringe:(float)fringe
                            text:(int)text;

- (int)renderUpdateTextureWithImage:(int)image
                                  x:(int)x
                                  y:(int)y
                              width:(int)width
                             height:(int)height
                               data:(const unsigned char*)data;

- (void)renderViewportWithWidth:(float)width
                         height:(float)height
               devicePixelRatio:(float)devicePixelRatio;

- (void)stroke:(MNVGcall*)call;

- (void)triangles:(MNVGcall*)call;

- (void)updateRenderPipelineStatesForBlend:(MNVGblend*)blend
                               pixelFormat:(MTLPixelFormat)pixelFormat;

@end

// Keeps the weak reference to the currently binded framebuffer.
MNVGframebuffer* s_framebuffer = NULL;

const MTLResourceOptions kMetalBufferOptions = \
(MTLResourceCPUCacheModeWriteCombined | MTLResourceStorageModeShared);

#if TARGET_OS_SIMULATOR
const MTLPixelFormat kStencilFormat = MTLPixelFormatDepth32Float_Stencil8;
#else
const MTLPixelFormat kStencilFormat = MTLPixelFormatStencil8;
#endif  // TARGET_OS_SIMULATOR

typedef enum {
    PACK_LINE_STYLE,
    PACK_TEX_TYPE,
    PACK_TYPE,
    PACK_REVERSE
} PackType;

int mtlnvg_packStateDataUniform(PackType packType, int value) {
    switch (packType) {
        case PACK_LINE_STYLE:
            return (value & 0x03) << 7;
        case PACK_TEX_TYPE:
            return (value & 0x03) << 5;
        case PACK_TYPE:
            return (value & 0x0F) << 1;
        case PACK_REVERSE:
            return value & 0x01;
        default:
            return 0;
    }
}

static BOOL mtlnvg_convertBlendFuncFactor(int factor, MTLBlendFactor* result) {
    if (factor == NVG_ZERO)
        *result = MTLBlendFactorZero;
    else if (factor == NVG_ONE)
        *result = MTLBlendFactorOne;
    else if (factor == NVG_SRC_COLOR)
        *result = MTLBlendFactorSourceColor;
    else if (factor == NVG_ONE_MINUS_SRC_COLOR)
        *result = MTLBlendFactorOneMinusSourceColor;
    else if (factor == NVG_DST_COLOR)
        *result = MTLBlendFactorDestinationColor;
    else if (factor == NVG_ONE_MINUS_DST_COLOR)
        *result = MTLBlendFactorOneMinusDestinationColor;
    else if (factor == NVG_SRC_ALPHA)
        *result = MTLBlendFactorSourceAlpha;
    else if (factor == NVG_ONE_MINUS_SRC_ALPHA)
        *result = MTLBlendFactorOneMinusSourceAlpha;
    else if (factor == NVG_DST_ALPHA)
        *result = MTLBlendFactorDestinationAlpha;
    else if (factor == NVG_ONE_MINUS_DST_ALPHA)
        *result = MTLBlendFactorOneMinusDestinationAlpha;
    else if (factor == NVG_SRC_ALPHA_SATURATE)
        *result = MTLBlendFactorSourceAlphaSaturated;
    else
        return NO;
    return YES;
}

static int mtlnvg__maxi(int a, int b) { return a > b ? a : b; }

static int mtlnvg__maxVertCount(const NVGpath* paths, int npaths,
                                int* indexCount, int* strokeCount) {
    int count = 0;
    if (indexCount != NULL) *indexCount = 0;
    if (strokeCount != NULL) *strokeCount = 0;
    NVGpath* path = (NVGpath*)&paths[0];
    for (int i = npaths; i--; ++path) {
        const int nfill = path->nfill;
        if (nfill > 2) {
            count += nfill;
            if (indexCount != NULL)
                *indexCount += (nfill - 2) * 3;
        }
        if (path->nstroke > 0) {
            const int nstroke = path->nstroke + 2;
            count += nstroke;
            if (strokeCount != NULL) *strokeCount += nstroke;
        }
    }
    return count;
}

static vector_float4 mtlnvg__premulColor(NVGcolor c) {
    c.r *= c.a;
    c.g *= c.a;
    c.b *= c.a;
    return (vector_float4){c.r, c.g, c.b, c.a};
}

static void mtlnvg__renderCancel(void* uptr) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    [mtl renderCancel];
}

static int mtlnvg__renderCreateTexture(void* uptr, int type, int width,
                                       int height, int imageFlags,
                                       const unsigned char* data) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    return [mtl renderCreateTextureWithType:type
                                      width:width
                                     height:height
                                 imageFlags:imageFlags
                                       data:data];
}

static int mtlnvg__renderCreate(void* uptr) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    return [mtl renderCreate];
}

static void mtlnvg__renderDelete(void* uptr) {
    MNVGcontext* mtl = (__bridge_transfer MNVGcontext*)uptr;
    [mtl renderDelete];
}

static int mtlnvg__renderDeleteTexture(void* uptr, int image) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    return [mtl renderDeleteTexture:image];
}

static void mtlnvg__renderFill(void* uptr, NVGpaint* paint,
                               NVGcompositeOperationState compositeOperation,
                               NVGscissor* scissor, float fringe,
                               const float* bounds, const NVGpath* paths,
                               int npaths) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    [mtl renderFillWithPaint:paint
          compositeOperation:compositeOperation
                     scissor:scissor
                      fringe:fringe
                      bounds:bounds
                       paths:paths
                      npaths:npaths];
}

static void mtlnvg__renderFlush(void* uptr) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
#if TARGET_OS_OSX
    [[mtl renderEncoder] textureBarrier];
#endif
    [mtl renderFlush];
}

static int mtlnvg__renderGetTextureSize(void* uptr, int image, int* w, int* h) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    return [mtl renderGetTextureSizeForImage:image width:w height:h];
}

static void mtlnvg__renderStroke(void* uptr, NVGpaint* paint,
                                 NVGcompositeOperationState compositeOperation,
                                 NVGscissor* scissor, float fringe,
                                 float strokeWidth, int lineStyle, float lineLength, const NVGpath* paths,
                                 int npaths) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    [mtl renderStrokeWithPaint:paint
            compositeOperation:compositeOperation
                       scissor:scissor
                        fringe:fringe
                   strokeWidth:strokeWidth
                     lineStyle:lineStyle
                    lineLength:lineLength
                         paths:paths
                        npaths:npaths];
}

static void mtlnvg__renderTriangles(
                                    void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation,
                                    NVGscissor* scissor, const NVGvertex* verts, int nverts, float fringe, int text) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    [mtl renderTrianglesWithPaint:paint
               compositeOperation:compositeOperation
                          scissor:scissor
                            verts:verts
                           nverts:nverts
                           fringe:fringe
                             text:text];
}

static int mtlnvg__renderUpdateTexture(void* uptr, int image, int x, int y,
                                       int w, int h,
                                       const unsigned char* data) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    return [mtl renderUpdateTextureWithImage:image
                                           x:x
                                           y:y
                                       width:w
                                      height:h
                                        data:data];
}

static void mtlnvg__renderViewport(void* uptr, float width, float height,
                                   float devicePixelRatio) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)uptr;
    [mtl renderViewportWithWidth:width
                          height:height
                devicePixelRatio:devicePixelRatio];
}

static void mtlnvg__xformToMat3x3(matrix_float3x3* m3, float* t) {
    *m3 = matrix_from_columns((vector_float3){t[0], t[1], 0.0f},
                              (vector_float3){t[2], t[3], 0.0f},
                              (vector_float3){t[4], t[5], 1.0f});
}

#if TARGET_OS_IPHONE

void mnvgSetViewBounds(void* view, int width, int height) {
    [(CAMetalLayer*)[(__bridge UIView*)view layer] setDrawableSize:CGSizeMake(width, height)];
}

NVGcontext* mnvgCreateContext(void* view, int flags, int width, int height) {
    CAMetalLayer *metalLayer = (CAMetalLayer*)[(__bridge UIView*)view layer];
    id<MTLDevice> metalDevice = MTLCreateSystemDefaultDevice();
    if (!metalDevice) return NULL;

    [metalLayer setPixelFormat:MTLPixelFormatRGBA8Unorm];
    [metalLayer setDevice: metalDevice];
    [metalLayer setDrawableSize:CGSizeMake(width, height)];
    return nvgCreateMTL((__bridge void*)metalLayer, flags);
}
#else
void mnvgSetViewBounds(void* view, int width, int height) {
    [(CAMetalLayer*)[(__bridge NSView*)view layer] setDrawableSize:CGSizeMake(width, height)];
}

NVGcontext* mnvgCreateContext(void* view, int flags, int width, int height) {
    CAMetalLayer *metalLayer = [CAMetalLayer new];
    id<MTLDevice> metalDevice = MTLCreateSystemDefaultDevice();
    if (!metalDevice) return NULL;

    ((__bridge NSView*) view).layer = metalLayer;
    [metalLayer setPixelFormat:MTLPixelFormatRGBA8Unorm];
    [metalLayer setDevice: metalDevice];
    [metalLayer setDrawableSize:CGSizeMake(width, height)];
    return nvgCreateMTL((__bridge void*)((__bridge NSView*) view).layer, flags);
}
#endif

NVGcontext* nvgCreateMTL(void* metalLayer, int flags) {
#ifdef MNVG_INVALID_TARGET
    printf("Metal is only supported on iOS, macOS, and tvOS.\n");
    return NULL;
#endif  // MNVG_INVALID_TARGET

    NVGparams params;
    NVGcontext* ctx = NULL;
    MNVGcontext* mtl = [MNVGcontext new];

    memset(&params, 0, sizeof(params));
    params.renderCreate = mtlnvg__renderCreate;
    params.renderCreateTexture = mtlnvg__renderCreateTexture;
    params.renderDeleteTexture = mtlnvg__renderDeleteTexture;
    params.renderUpdateTexture = mtlnvg__renderUpdateTexture;
    params.renderGetTextureSize = mtlnvg__renderGetTextureSize;
    params.renderViewport = mtlnvg__renderViewport;
    params.renderCancel = mtlnvg__renderCancel;
    params.renderFlush = mtlnvg__renderFlush;
    params.renderFill = mtlnvg__renderFill;
    params.renderStroke = mtlnvg__renderStroke;
    params.renderTriangles = mtlnvg__renderTriangles;
    params.renderDelete = mtlnvg__renderDelete;
    params.userPtr = (__bridge_retained void*)mtl;
    params.edgeAntiAlias = flags & NVG_ANTIALIAS ? 1 : 0;

    mtl.flags = flags;
#if __aarch64__ && !TARGET_OS_SIMULATOR
    mtl.fragSize = sizeof(MNVGfragUniforms);
#else
    mtl.fragSize = 256;
#endif
    mtl.lastUniformOffset = 0;
    mtl.lastBoundTexture = -1;
    mtl.indexSize = 4;  // MTLIndexTypeUInt32
    mtl.metalLayer = (__bridge CAMetalLayer*)metalLayer;

    ctx = nvgCreateInternal(&params);
    if (ctx == NULL) goto error;
    return ctx;

error:
    // 'mtl' is freed by nvgDeleteInternal.
    if (ctx != NULL) nvgDeleteInternal(ctx);
    return NULL;
}

static void mtlnvg__vset(NVGvertex* vtx, float x, float y, float u, float v) {
    int16_t scaling_factor = 1 << 14;
    vtx->x = x;
    vtx->y = y;
    vtx->u = u * scaling_factor;
    vtx->v = v * scaling_factor;
}

void nvgDeleteMTL(NVGcontext* ctx) {
    nvgDeleteInternal(ctx);
}

void mnvgBindFramebuffer(MNVGframebuffer* framebuffer) {
    s_framebuffer = framebuffer;
}

MNVGframebuffer* mnvgCreateFramebuffer(NVGcontext* ctx, int width,
                                       int height, int imageFlags) {
    MNVGframebuffer* framebuffer = \
    (MNVGframebuffer*)malloc(sizeof(MNVGframebuffer));
    if (framebuffer == NULL)
        return NULL;

    memset(framebuffer, 0, sizeof(MNVGframebuffer));
    framebuffer->image = nvgCreateImageRGBA(ctx, width, height,
                                            imageFlags | NVG_IMAGE_PREMULTIPLIED,
                                            NULL);
    framebuffer->ctx = ctx;
    return framebuffer;
}

void mnvgDeleteFramebuffer(MNVGframebuffer* framebuffer) {
    if (framebuffer == NULL) return;
    if (framebuffer->image > 0) {
        nvgDeleteImage(framebuffer->ctx, framebuffer->image);
    }
    free(framebuffer);
}

void mnvgClearWithColor(NVGcontext* ctx, NVGcolor color) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)nvgInternalParams(ctx)->userPtr;
    float alpha = (float)color.a;
    mtl.clearColor = MTLClearColorMake((float)color.r * alpha,
                                       (float)color.g * alpha,
                                       (float)color.b * alpha,
                                       (float)color.a);
    mtl.clearBufferOnFlush = YES;
}

void* mnvgDevice(NVGcontext* ctx) {
    MNVGcontext* mtl = (__bridge MNVGcontext*)nvgInternalParams(ctx)->userPtr;
    return (__bridge void*)mtl.metalLayer.device;
}

@implementation MNVGbuffers
@end

@implementation MNVGcontext

- (MNVGcall*)allocCall {
    MNVGcall* ret = NULL;
    MNVGrenderData* renderData = _buffers.renderData;
    if (renderData->ncalls + 1 > renderData->ccalls) {
        MNVGcall* calls;
        int ccalls = mtlnvg__maxi(renderData->ncalls + 1, 128) + renderData->ccalls / 2;
        calls = (MNVGcall*)realloc(renderData->calls, sizeof(MNVGcall) * ccalls);
        if (calls == NULL) return NULL;
        renderData->calls = calls;
        renderData->ccalls = ccalls;
    }
    ret = &renderData->calls[renderData->ncalls++];
    memset(ret, 0, sizeof(MNVGcall));
    return ret;
}

- (int)allocFragUniforms:(int)n {
    MNVGrenderData* renderData = _buffers.renderData;
    int ret = 0;
    if (renderData->nuniforms + n > renderData->cuniforms) {
        int cuniforms = mtlnvg__maxi(renderData->nuniforms + n, 128) + renderData->cuniforms / 2;
        id<MTLBuffer> buffer = [_metalLayer.device
                                newBufferWithLength:(_fragSize * cuniforms)
                                options:kMetalBufferOptions];
        unsigned char* uniforms = [buffer contents];
        if (_buffers.uniformBuffer != nil) {
            memcpy(uniforms, renderData->uniforms,
                   _fragSize * renderData->nuniforms);
        }
        _buffers.uniformBuffer = buffer;
        renderData->uniforms = uniforms;
        renderData->cuniforms = cuniforms;
    }
    ret = renderData->nuniforms * _fragSize;
    renderData->nuniforms += n;
    return ret;
}

- (int)allocIndexes:(int)n {
    int ret = 0;
    MNVGrenderData* renderData = _buffers.renderData;
    if (renderData->nindexes + n > renderData->cindexes) {
        int cindexes = mtlnvg__maxi(renderData->nindexes + n, 4096) \
        + renderData->cindexes / 2;
        id<MTLBuffer> buffer = [_metalLayer.device
                                newBufferWithLength:(_indexSize * cindexes)
                                options:kMetalBufferOptions];
        uint32_t* indexes = [buffer contents];
        if (_buffers.indexBuffer != nil) {
            memcpy(indexes, renderData->indexes, _indexSize * renderData->nindexes);
        }
        _buffers.indexBuffer = buffer;
        renderData->indexes = indexes;
        renderData->cindexes = cindexes;
    }
    ret = renderData->nindexes;
    renderData->nindexes += n;
    return ret;
}

- (MNVGtexture*)allocTexture {
    MNVGtexture* tex = nil;

    for (MNVGtexture* texture in _textures) {
        if (texture->valid == 0) {
            tex = texture;
            break;
        }
    }
    if (tex == nil) {
        tex = [MNVGtexture new];
        [_textures addObject:tex];
        tex->id = ++_textureId;
    }
    tex->valid = true;
    return tex;
}

- (int)allocVerts:(int)n {
    MNVGrenderData* renderData = _buffers.renderData;
    int ret = 0;
    if (renderData->nverts + n > renderData->cverts) {
        int cverts = mtlnvg__maxi(renderData->nverts + n, 4096) + renderData->cverts / 2;
        id<MTLBuffer> buffer = [_metalLayer.device
                                newBufferWithLength:(sizeof(NVGvertex) * cverts)
                                options:kMetalBufferOptions];
        NVGvertex* verts = [buffer contents];
        if (_buffers.vertBuffer != nil) {
            memcpy(verts, renderData->verts, sizeof(NVGvertex) * renderData->nverts);
        }
        _buffers.vertBuffer = buffer;
        renderData->verts = verts;
        renderData->cverts = cverts;
    }
    ret = renderData->nverts;
    renderData->nverts += n;
    return ret;
}

- (MNVGblend)blendCompositeOperation:(NVGcompositeOperationState)op {
    MNVGblend blend;
    if (!mtlnvg_convertBlendFuncFactor(op.srcRGB, &blend.srcRGB) ||
        !mtlnvg_convertBlendFuncFactor(op.dstRGB, &blend.dstRGB) ||
        !mtlnvg_convertBlendFuncFactor(op.srcAlpha, &blend.srcAlpha) ||
        !mtlnvg_convertBlendFuncFactor(op.dstAlpha, &blend.dstAlpha)) {
        blend.srcRGB = MTLBlendFactorOne;
        blend.dstRGB = MTLBlendFactorOneMinusSourceAlpha;
        blend.srcAlpha = MTLBlendFactorOne;
        blend.dstAlpha = MTLBlendFactorOneMinusSourceAlpha;
    }
    return blend;
}

- (void)checkError:(NSError*)error withMessage:(const char*)message {
    if ((_flags & NVG_DEBUG) == 0) return;
    if (error) {
        printf("Error occurs after %s: %s\n",
               message, [[error localizedDescription] UTF8String]);
    }
}

- (int)convertPaintForFrag:(MNVGfragUniforms*)frag
                     paint:(NVGpaint*)paint
                   scissor:(NVGscissor*)scissor
                     width:(float)width
                    fringe:(float)fringe
                 lineStyle:(int)lineStyle
                lineLength:(float)lineLength
              lineReversed: (int)lineReversed {
    MNVGtexture* tex = nil;
    float invxform[6];
    int is_gradient = memcmp(&(paint->innerColor), &(paint->outerColor), sizeof(paint->outerColor));

    memset(frag, 0, sizeof(*frag));

    frag->innerCol = mtlnvg__premulColor(paint->innerColor);
    frag->outerCol = mtlnvg__premulColor(paint->outerColor);
    frag->dashCol = mtlnvg__premulColor(paint->dashColor);
    frag->stateData = mtlnvg_packStateDataUniform(PACK_LINE_STYLE, lineStyle);
    frag->radius = paint->radius;

    if (scissor->extent[0] < -0.5f || scissor->extent[1] < -0.5f) {
        frag->scissorMat = matrix_from_rows((vector_float3){0, 0, 0},
                                            (vector_float3){0, 0, 0},
                                            (vector_float3){0, 0, 0});
        frag->scissorExt.x = 1.0f;
        frag->scissorExt.y = 1.0f;
        frag->scissorScale.x = 1.0f;
        frag->scissorScale.y = 1.0f;
        frag->scissorRadius = 0.0f;
    } else {
        nvgTransformInverse(invxform, scissor->xform);
        mtlnvg__xformToMat3x3(&frag->scissorMat, invxform);
        frag->scissorExt.x = scissor->extent[0];
        frag->scissorExt.y = scissor->extent[1];
        frag->scissorScale.x = sqrtf(scissor->xform[0] * scissor->xform[0] + scissor->xform[2] * scissor->xform[2]) / fringe;
        frag->scissorScale.y = sqrtf(scissor->xform[1] * scissor->xform[1] + scissor->xform[3] * scissor->xform[3]) / fringe;
        frag->scissorRadius = scissor->radius;
    }

    frag->extent = (vector_float2){paint->extent[0], paint->extent[1]};
    frag->strokeMult = (width * 0.5f + fringe * 0.5f) / fringe;

    if (paint->image != 0) {
        tex = [self findTexture:paint->image];
        if (tex == nil) return 0;
        if (tex->flags & NVG_IMAGE_FLIPY) {
            float m1[6], m2[6];
            nvgTransformTranslate(m1, 0.0f, frag->extent.y * 0.5f);
            nvgTransformMultiply(m1, paint->xform);
            nvgTransformScale(m2, 1.0f, -1.0f);
            nvgTransformMultiply(m2, m1);
            nvgTransformTranslate(m1, 0.0f, -frag->extent.y * 0.5f);
            nvgTransformMultiply(m1, m2);
            nvgTransformInverse(invxform, m1);
        } else {
            nvgTransformInverse(invxform, paint->xform);
        }
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_FILLIMG);

        if (tex->type == NVG_TEXTURE_RGBA)
            frag->stateData |= mtlnvg_packStateDataUniform(PACK_TEX_TYPE, (tex->flags & NVG_IMAGE_PREMULTIPLIED) ? 0 : 1);
        else
            frag->stateData |= mtlnvg_packStateDataUniform(PACK_TEX_TYPE, 2);
    } else if(paint->rounded_rect) {
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_FAST_ROUNDEDRECT);
        nvgTransformInverse(invxform, paint->xform);
        frag->scissorExt[0] = scissor->extent[0];
        frag->scissorExt[1] = scissor->extent[1];
        frag->scissorScale[0] = sqrtf(scissor->xform[0]*scissor->xform[0] + scissor->xform[2]*scissor->xform[2]) / fringe;
        frag->scissorScale[1] = sqrtf(scissor->xform[1]*scissor->xform[1] + scissor->xform[3]*scissor->xform[3]) / fringe;
        frag->radius = paint->radius;
    } else if(paint->double_stroke) {
        if (paint->gradient_stroke) {
            if (paint->connection_activity) {
                frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY);
                frag->offset = paint->offset;
            } else {
                frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_DOUBLE_STROKE_GRAD);
            }
        } else {
            if (paint->connection_activity){
                frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_DOUBLE_STROKE_ACTIVITY);
                frag->offset = paint->offset;
            } else {
                frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_DOUBLE_STROKE);
            }
        }
        frag->lineLength = lineLength;
        frag->feather = paint->feather;
        frag->radius = paint->radius;
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_REVERSE, lineReversed);
        nvgTransformInverse(invxform, paint->xform);
    }
    else if(paint->smooth_glow) {
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_SMOOTH_GLOW);
        frag->radius = paint->radius;
        frag->feather = paint->feather;
        frag->scissorExt[0] = scissor->extent[0];
        frag->scissorExt[1] = scissor->extent[1];
        frag->scissorScale[0] = sqrtf(scissor->xform[0]*scissor->xform[0] + scissor->xform[2]*scissor->xform[2]) / fringe;
        frag->scissorScale[1] = sqrtf(scissor->xform[1]*scissor->xform[1] + scissor->xform[3]*scissor->xform[3]) / fringe;
        nvgTransformInverse(invxform, paint->xform);
    }
    else if(paint->dots) {
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_DOTS);
        frag->feather = paint->feather;
        frag->patternSize = paint->dot_pattern_size;
        frag->radius = paint->radius;
        nvgTransformInverse(invxform, paint->xform);
    } else if (paint->image == 0 && lineStyle == NVG_LINE_SOLID && !is_gradient) {
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_FILLCOLOR);
        nvgTransformInverse(invxform, paint->xform);
    } else {
        frag->stateData |= mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_FILLGRAD);
        frag->radius = paint->radius;
        frag->feather = paint->feather;
        frag->lineLength = lineLength;
        nvgTransformInverse(invxform, paint->xform);
    }

    mtlnvg__xformToMat3x3(&frag->paintMat, invxform);

    return 1;
}

- (void)convexFill:(MNVGcall*)call {
    const int kIndexBufferOffset = call->indexOffset * _indexSize;
    [self setUniforms:call->uniformOffset image:call->image];
    if (call->indexCount > 0) {
        [_renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:call->indexCount
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:_buffers.indexBuffer
                            indexBufferOffset:kIndexBufferOffset];
    }

    // Draw fringes
    if (call->strokeCount > 0) {
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];
    }
}

-(void)fill:(MNVGcall*)call {
    // Draws shapes.
    const int kIndexBufferOffset = call->indexOffset * _indexSize;
    [_renderEncoder setCullMode:MTLCullModeNone];
    [_renderEncoder setDepthStencilState:_fillShapeStencilState];
    [_renderEncoder setRenderPipelineState:_stencilOnlyPipelineState];
    if (call->indexCount > 0) {
        [_renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:call->indexCount
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:_buffers.indexBuffer
                            indexBufferOffset:kIndexBufferOffset];
    }

    // Restores states.
    [_renderEncoder setCullMode:MTLCullModeBack];
    [_renderEncoder setRenderPipelineState:_pipelineState];

    // Draws anti-aliased fragments.
    [self setUniforms:call->uniformOffset image:call->image];
    if (_flags & NVG_ANTIALIAS && call->strokeCount > 0) {
        [_renderEncoder setDepthStencilState:_fillAntiAliasStencilState];
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];
    }

    // Draws fill.
    [_renderEncoder setDepthStencilState:_fillStencilState];
    [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                       vertexStart:call->triangleOffset
                       vertexCount:call->triangleCount];
    [_renderEncoder setDepthStencilState:_defaultStencilState];
}

- (MNVGtexture* __unsafe_unretained)findTexture:(int)id {
    if (id <= 0) return nil;
    return _textures[id - 1];
}

- (MNVGfragUniforms*)fragUniformAtIndex:(int)index {
    return (MNVGfragUniforms*)&_buffers.renderData->uniforms[index];
}

- (void)renderCancel {
    MNVGrenderData* renderData = _buffers.renderData;
    renderData->image = 0;
    _buffers.isBusy = NO;
    renderData->nindexes = 0;
    renderData->nverts = 0;
    renderData->ncalls = 0;
    renderData->nuniforms = 0;
    dispatch_semaphore_signal(_semaphore);
}

- (id<MTLRenderCommandEncoder>)renderCommandEncoderWithColorTexture:
(id<MTLTexture>)colorTexture {
    MTLRenderPassDescriptor *descriptor = \
    [MTLRenderPassDescriptor renderPassDescriptor];
    if (descriptor == nil) {
        return nil;
    }

    descriptor.colorAttachments[0].clearColor = _clearColor;
    descriptor.colorAttachments[0].loadAction = \
    _clearBufferOnFlush ? MTLLoadActionClear : MTLLoadActionLoad;
    descriptor.colorAttachments[0].storeAction = MTLStoreActionStore;
    descriptor.colorAttachments[0].texture = colorTexture;
    _clearBufferOnFlush = NO;

     descriptor.stencilAttachment.clearStencil = 0;
     descriptor.stencilAttachment.loadAction = MTLLoadActionClear;
     descriptor.stencilAttachment.storeAction = MTLStoreActionDontCare;
     descriptor.stencilAttachment.texture = _buffers.stencilTexture;

    id<MTLCommandBuffer> commandBuffer = _buffers.commandBuffer;
    id<MTLRenderCommandEncoder> encoder = [commandBuffer
                                           renderCommandEncoderWithDescriptor:descriptor];

    [encoder setCullMode:MTLCullModeBack];
    [encoder setFrontFacingWinding:MTLWindingCounterClockwise];
    [encoder setStencilReferenceValue:0];
    [encoder setViewport:(MTLViewport)
     {0.0, 0.0, _viewPortSize.x, _viewPortSize.y, 0.0, 1.0}];

    [encoder setVertexBuffer:_buffers.vertBuffer
                      offset:0
                     atIndex:MNVG_VERTEX_INPUT_INDEX_VERTICES];

    [encoder setVertexBuffer:_buffers.viewSizeBuffer
                      offset:0
                     atIndex:MNVG_VERTEX_INPUT_INDEX_VIEW_SIZE];

    [encoder setFragmentBuffer:_buffers.uniformBuffer offset:0 atIndex:0];

    return encoder;
}

- (int)renderCreate {
    if (_metalLayer.device == nil) {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        _metalLayer.device = device;
    }

    // Loads shaders from pre-compiled metal library..
    NSError* error;
    id<MTLDevice> device = _metalLayer.device;
#ifdef MNVG_INVALID_TARGET
    id<MTLLibrary> library = nil;
    return 0;
#endif

    unsigned char* metal_library_bitcode;
    unsigned int metal_library_bitcode_len;
#if TARGET_OS_SIMULATOR
    metal_library_bitcode = mnvg_bitcode_simulator;
    metal_library_bitcode_len = mnvg_bitcode_simulator_len;
#elif TARGET_OS_IOS
    if (@available(iOS 8, *)) {
    } else {
        return 0;
    }
    metal_library_bitcode = mnvg_bitcode_ios;
    metal_library_bitcode_len = mnvg_bitcode_ios_len;
#elif TARGET_OS_OSX
    if (@available(macOS 10.11, *)) {
        metal_library_bitcode = mnvg_bitcode_macos;
        metal_library_bitcode_len = mnvg_bitcode_macos_len;
    } else {
        return 0;
    }
#elif TARGET_OS_TV
    metal_library_bitcode = mnvg_bitcode_tvos;
    metal_library_bitcode_len = mnvg_bitcode_tvos_len;
#endif

    dispatch_data_t data = dispatch_data_create(metal_library_bitcode,
                                                metal_library_bitcode_len,
                                                NULL,
                                                DISPATCH_DATA_DESTRUCTOR_DEFAULT);
    id<MTLLibrary> library = [device newLibraryWithData:data error:&error];

    [self checkError:error withMessage:"init library"];
    if (library == nil) {
        return 0;
    }

    _vertexFunction = [library newFunctionWithName:@"vertexShader"];
    if (_flags & NVG_ANTIALIAS) {
        _fragmentFunction = [library newFunctionWithName:@"fragmentShaderAA"];
    } else {
        _fragmentFunction = [library newFunctionWithName:@"fragmentShader"];
    }

    _commandQueue = [device newCommandQueue];

    // Initializes the number of available buffers.
    if (_flags & NVG_TRIPLE_BUFFER) {
        _maxBuffers = 3;
    } else if (_flags & NVG_DOUBLE_BUFFER) {
        _maxBuffers = 2;
    } else {
        _maxBuffers = 1;
    }
    _cbuffers = [NSMutableArray arrayWithCapacity:_maxBuffers];
    for (int i = _maxBuffers; i--;) {
        MNVGbuffers *buffer = [MNVGbuffers new];
        buffer.renderData = malloc(sizeof(MNVGrenderData));
        memset(buffer.renderData, 0, sizeof(MNVGrenderData));
        [_cbuffers addObject:buffer];
    }
    _clearBufferOnFlush = NO;
    _semaphore = dispatch_semaphore_create(_maxBuffers);

    // Initializes vertex descriptor.
    _vertexDescriptor = [MTLVertexDescriptor vertexDescriptor];
    _vertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
    _vertexDescriptor.attributes[0].bufferIndex = 0;
    _vertexDescriptor.attributes[0].offset = offsetof(NVGvertex, x);

    _vertexDescriptor.attributes[1].format = MTLVertexFormatShort4Normalized;
    _vertexDescriptor.attributes[1].bufferIndex = 0;
    _vertexDescriptor.attributes[1].offset = offsetof(NVGvertex, u);

    _vertexDescriptor.layouts[0].stride = sizeof(NVGvertex);
    _vertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

    // Initialzes textures.
    _textureId = 0;
    _textures = [NSMutableArray array];

    MTLSamplerDescriptor* samplerDescriptor = [MTLSamplerDescriptor new];
     _pseudoSampler = [_metalLayer.device
         newSamplerStateWithDescriptor:samplerDescriptor];

    // Initializes pseudo texture
    const int kPseudoTextureImage = [self
                                     renderCreateTextureWithType:NVG_TEXTURE_ALPHA
                                     width:1
                                     height:1
                                     imageFlags:0
                                     data:NULL];
    MNVGtexture* tex = [self findTexture:kPseudoTextureImage];
    _pseudoTexture = tex->tex;

    // Initializes default blend states.
    _blendFunc = malloc(sizeof(MNVGblend));
    _blendFunc->srcRGB = MTLBlendFactorOne;
    _blendFunc->dstRGB = MTLBlendFactorOneMinusSourceAlpha;
    _blendFunc->srcAlpha = MTLBlendFactorOne;
    _blendFunc->dstAlpha = MTLBlendFactorOneMinusSourceAlpha;

    // Initializes stencil states.
    MTLDepthStencilDescriptor* stencilDescriptor = \
    [MTLDepthStencilDescriptor new];

    // Default stencil state.
    _defaultStencilState = [device
                            newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Fill shape stencil.
    MTLStencilDescriptor* frontFaceStencilDescriptor = [MTLStencilDescriptor new];
    frontFaceStencilDescriptor.stencilCompareFunction = MTLCompareFunctionAlways;
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationIncrementWrap;

    MTLStencilDescriptor* backFaceStencilDescriptor = [MTLStencilDescriptor new];
    backFaceStencilDescriptor.stencilCompareFunction = MTLCompareFunctionAlways;
    backFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationDecrementWrap;

    stencilDescriptor.depthCompareFunction = MTLCompareFunctionAlways;
    stencilDescriptor.backFaceStencil = backFaceStencilDescriptor;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _fillShapeStencilState = [device
                              newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Fill anti-aliased stencil.
    frontFaceStencilDescriptor.stencilCompareFunction = MTLCompareFunctionEqual;
    frontFaceStencilDescriptor.stencilFailureOperation = MTLStencilOperationKeep;
    frontFaceStencilDescriptor.depthFailureOperation = MTLStencilOperationKeep;
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationZero;

    stencilDescriptor.backFaceStencil = nil;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _fillAntiAliasStencilState = [device
                                  newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Fill stencil.
    frontFaceStencilDescriptor.stencilCompareFunction = \
    MTLCompareFunctionNotEqual;
    frontFaceStencilDescriptor.stencilFailureOperation = MTLStencilOperationZero;
    frontFaceStencilDescriptor.depthFailureOperation = MTLStencilOperationZero;
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationZero;

    stencilDescriptor.backFaceStencil = nil;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _fillStencilState = [device
                         newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Stroke shape stencil.
    frontFaceStencilDescriptor.stencilCompareFunction = MTLCompareFunctionEqual;
    frontFaceStencilDescriptor.stencilFailureOperation = MTLStencilOperationKeep;
    frontFaceStencilDescriptor.depthFailureOperation = MTLStencilOperationKeep;
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationIncrementClamp;

    stencilDescriptor.backFaceStencil = nil;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _strokeShapeStencilState = [device
                                newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Stroke anti-aliased stencil.
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationKeep;

    stencilDescriptor.backFaceStencil = nil;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _strokeAntiAliasStencilState = [device
                                    newDepthStencilStateWithDescriptor:stencilDescriptor];

    // Stroke clear stencil.
    frontFaceStencilDescriptor.stencilCompareFunction = MTLCompareFunctionAlways;
    frontFaceStencilDescriptor.stencilFailureOperation = MTLStencilOperationZero;
    frontFaceStencilDescriptor.depthFailureOperation = MTLStencilOperationZero;
    frontFaceStencilDescriptor.depthStencilPassOperation = \
    MTLStencilOperationZero;

    stencilDescriptor.backFaceStencil = nil;
    stencilDescriptor.frontFaceStencil = frontFaceStencilDescriptor;
    _strokeClearStencilState = [device
                                newDepthStencilStateWithDescriptor:stencilDescriptor];

    return 1;
}

- (int)renderCreateTextureWithType:(int)type
                             width:(int)width
                            height:(int)height
                        imageFlags:(int)imageFlags
                              data:(const unsigned char*)data {
    MNVGtexture* tex = [self allocTexture];

    if (tex == nil) return 0;

    MTLPixelFormat pixelFormat = MTLPixelFormatRGBA8Unorm;
    if (type == NVG_TEXTURE_ALPHA) {
        pixelFormat = MTLPixelFormatR8Unorm;
    }

    tex->type = type;
    tex->flags = imageFlags;

    MTLTextureDescriptor *textureDescriptor = [MTLTextureDescriptor
                                               texture2DDescriptorWithPixelFormat:pixelFormat
                                               width:width
                                               height:height
                                               mipmapped:(imageFlags & NVG_IMAGE_GENERATE_MIPMAPS ? YES : NO)];
    textureDescriptor.usage = MTLTextureUsageShaderRead
    | MTLTextureUsageRenderTarget;
#if TARGET_OS_SIMULATOR
    textureDescriptor.storageMode = MTLStorageModePrivate;
#endif  // TARGET_OS_SIMULATOR
    tex->tex = [_metalLayer.device newTextureWithDescriptor:textureDescriptor];

    if (data != NULL) {
        NSUInteger bytesPerRow;
        if (tex->type == NVG_TEXTURE_RGBA) {
            bytesPerRow = width * 4;
        } else {
            bytesPerRow = width;
        }

        if (textureDescriptor.storageMode == MTLStorageModePrivate) {
            const NSUInteger kBufferSize = bytesPerRow * height;
            id<MTLBuffer> buffer = [_metalLayer.device
                                    newBufferWithLength:kBufferSize
                                    options:MTLResourceStorageModeShared];
            memcpy([buffer contents], data, kBufferSize);

            id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
            id<MTLBlitCommandEncoder> blitCommandEncoder = [commandBuffer
                                                            blitCommandEncoder];
            [blitCommandEncoder copyFromBuffer:buffer
                                  sourceOffset:0
                             sourceBytesPerRow:bytesPerRow
                           sourceBytesPerImage:kBufferSize
                                    sourceSize:MTLSizeMake(width, height, 1)
                                     toTexture:tex->tex
                              destinationSlice:0
                              destinationLevel:0
                             destinationOrigin:MTLOriginMake(0, 0, 0)];

            [blitCommandEncoder endEncoding];
            [commandBuffer commit];
            [commandBuffer waitUntilCompleted];
        } else {
            [tex->tex replaceRegion:MTLRegionMake2D(0, 0, width, height)
                        mipmapLevel:0
                          withBytes:data
                        bytesPerRow:bytesPerRow];
        }

        if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS) {
            id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
            id<MTLBlitCommandEncoder> encoder = [commandBuffer blitCommandEncoder];
            [encoder generateMipmapsForTexture:tex->tex];
            [encoder endEncoding];
            [commandBuffer commit];
            [commandBuffer waitUntilCompleted];
        }
    }

    MTLSamplerDescriptor* samplerDescriptor = [MTLSamplerDescriptor new];
    if (imageFlags & NVG_IMAGE_NEAREST) {
        samplerDescriptor.minFilter = MTLSamplerMinMagFilterNearest;
        samplerDescriptor.magFilter = MTLSamplerMinMagFilterNearest;
        if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS)
            samplerDescriptor.mipFilter = MTLSamplerMipFilterNearest;
    } else {
        samplerDescriptor.minFilter = MTLSamplerMinMagFilterLinear;
        samplerDescriptor.magFilter = MTLSamplerMinMagFilterLinear;
        if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS)
            samplerDescriptor.mipFilter = MTLSamplerMipFilterLinear;
    }

    if (imageFlags & NVG_IMAGE_REPEATX) {
        samplerDescriptor.sAddressMode = MTLSamplerAddressModeRepeat;
    } else {
        samplerDescriptor.sAddressMode = MTLSamplerAddressModeClampToEdge;
    }

    if (imageFlags & NVG_IMAGE_REPEATY) {
        samplerDescriptor.tAddressMode = MTLSamplerAddressModeRepeat;
    } else {
        samplerDescriptor.tAddressMode = MTLSamplerAddressModeClampToEdge;
    }

    tex->sampler = [_metalLayer.device
                    newSamplerStateWithDescriptor:samplerDescriptor];

    return tex->id;
}

- (void)renderDelete {

    [self renderCancel];

    for (MNVGbuffers* buffers in _cbuffers) {
        buffers.commandBuffer = nil;
        buffers.viewSizeBuffer = nil;
        buffers.stencilTexture = nil;
        buffers.indexBuffer = nil;
        buffers.vertBuffer = nil;
        buffers.uniformBuffer = nil;
        free(buffers.renderData->calls);
    }

    for (MNVGtexture* texture in _textures) {
        texture->tex = nil;
        texture->sampler = nil;
    }

    free(_blendFunc);
    _commandQueue = nil;
    _renderEncoder = nil;
    _textures = nil;
    _cbuffers = nil;
    _defaultStencilState = nil;
    _fillShapeStencilState = nil;
    _fillAntiAliasStencilState = nil;
    _strokeShapeStencilState = nil;
    _strokeAntiAliasStencilState = nil;
    _strokeClearStencilState = nil;
    _fragmentFunction = nil;
    _vertexFunction = nil;
    _pipelineState = nil;
    _stencilOnlyPipelineState = nil;
    _pseudoTexture = nil;
    _vertexDescriptor = nil;
    _metalLayer.device = nil;
    _metalLayer = nil;
}

- (int)renderDeleteTexture:(int)image {
    if(image <= 0) return 0;
    MNVGtexture* texture = _textures[image-1];
    if(texture == nil) return 0;

    if (texture->tex != nil &&
        (texture->flags & NVG_IMAGE_NODELETE) == 0) {
        texture->tex = nil;
        texture->sampler = nil;
    }
    texture->valid = 0;
    texture->flags = 0;
    return 1;
}

- (void)renderFillWithPaint:(NVGpaint*)paint
         compositeOperation:(NVGcompositeOperationState)compositeOperation
                    scissor:(NVGscissor*)scissor
                     fringe:(float)fringe
                     bounds:(const float*)bounds
                      paths:(const NVGpath*)paths
                     npaths:(int)npaths {
    MNVGcall* call = [self allocCall];
    if (call == NULL) return;

    NVGvertex* quad;
    MNVGrenderData* renderData = _buffers.renderData;

    call->type = MNVG_FILL;
    call->triangleCount = 4;
    call->image = paint->image;
    call->blendFunc = [self blendCompositeOperation:compositeOperation];

    if (npaths == 1 && paths[0].convex) {
        call->type = MNVG_CONVEXFILL;
        call->triangleCount = 0;  // Bounding box fill quad not needed for convex fill
    }

    // Allocate vertices for all the paths.
    int indexCount, strokeCount = 0;
    int maxverts = mtlnvg__maxVertCount(paths, npaths, &indexCount, &strokeCount)
    + call->triangleCount;
    int vertOffset = [self allocVerts:maxverts];
    if (vertOffset == -1) goto error;

    int indexOffset = [self allocIndexes:indexCount];
    if (indexOffset == -1) goto error;
    call->indexOffset = indexOffset;
    call->indexCount = indexCount;
    uint32_t* index = &renderData->indexes[indexOffset];

    int strokeVertOffset = vertOffset + (maxverts - strokeCount);
    call->strokeOffset = strokeVertOffset + 1;
    call->strokeCount = strokeCount - 2;
    NVGvertex* strokeVert = renderData->verts + strokeVertOffset;

    NVGpath* path = (NVGpath*)&paths[0];
    for (int i = npaths; i--; ++path) {
        if (path->nfill > 2) {
            memcpy(&renderData->verts[vertOffset], path->fill,
                   sizeof(NVGvertex) * path->nfill);

            int hubVertOffset = vertOffset++;
            for (int j = 2; j < path->nfill; j++) {
                *index++ = hubVertOffset;
                *index++ = vertOffset++;
                *index++ = vertOffset;
            }
            vertOffset++;
        }
        if (path->nstroke > 0) {
            memcpy(strokeVert, path->stroke, sizeof(NVGvertex));
            ++strokeVert;
            memcpy(strokeVert, path->stroke, sizeof(NVGvertex) * path->nstroke);
            strokeVert += path->nstroke;
            memcpy(strokeVert, path->stroke + path->nstroke - 1, sizeof(NVGvertex));
            ++strokeVert;
        }
    }

    // Setup uniforms for draw calls
    if (call->type == MNVG_FILL) {
        // Quad
        call->triangleOffset = vertOffset;
        quad = &renderData->verts[call->triangleOffset];
        mtlnvg__vset(&quad[0], bounds[2], bounds[3], 0.5f, 1.0f);
        mtlnvg__vset(&quad[1], bounds[2], bounds[1], 0.5f, 1.0f);
        mtlnvg__vset(&quad[2], bounds[0], bounds[3], 0.5f, 1.0f);
        mtlnvg__vset(&quad[3], bounds[0], bounds[1], 0.5f, 1.0f);
    }

    // Fill shader
    call->uniformOffset = [self allocFragUniforms:1];
    if (call->uniformOffset == -1) goto error;
    [self convertPaintForFrag:[self fragUniformAtIndex:call->uniformOffset]
                        paint:paint
                      scissor:scissor
                        width:fringe
                       fringe:fringe
                    lineStyle:NVG_LINE_SOLID
                   lineLength:0.0f
                 lineReversed:0];
    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (renderData->ncalls > 0) renderData->ncalls--;
}

- (void)renderFlush {
    // Cancelled if the drawable is invisible.
    if (_viewPortSize.x == 0 || _viewPortSize.y == 0) {
        [self renderCancel];
        return;
    }

    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    id<MTLTexture> colorTexture = nil;
    vector_uint2 textureSize;
    _buffers.commandBuffer = commandBuffer;
    __block MNVGbuffers* buffers = _buffers;
    __weak id weakSelf = self;
    __weak MNVGbuffers* weakBuffers = buffers;

    [commandBuffer enqueue];
    [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> buffer) {
        if(weakBuffers) {

            MNVGrenderData* renderData = [weakBuffers renderData];
            renderData->image = 0;
            renderData->nindexes = 0;
            renderData->nverts = 0;
            renderData->ncalls = 0;
            renderData->nuniforms = 0;
            [weakBuffers setIsBusy:NO];
            [weakBuffers setCommandBuffer:nil];
        }
        if (weakSelf) {
            dispatch_semaphore_signal([weakSelf semaphore]);
        }
    }];

    MNVGrenderData* renderData = _buffers.renderData;
    if (s_framebuffer == NULL ||
        nvgInternalParams(s_framebuffer->ctx)->userPtr != (__bridge void*)self) {
        textureSize = _viewPortSize;
    } else {  // renders in framebuffer
        renderData->image = s_framebuffer->image;
        MNVGtexture* tex = [self findTexture:s_framebuffer->image];
        colorTexture = tex->tex;
        textureSize = (vector_uint2){(uint)colorTexture.width,
            (uint)colorTexture.height};
    }
    if (textureSize.x == 0 || textureSize.y == 0) return;
    [self updateStencilTextureToSize:&textureSize];

    id<CAMetalDrawable> drawable = nil;
    if (colorTexture == nil) {
        drawable = _metalLayer.nextDrawable;
        colorTexture = drawable.texture;
    }

    _renderEncoder = [self renderCommandEncoderWithColorTexture:colorTexture];
    if(_pipelineState != nil) [_renderEncoder setRenderPipelineState:_pipelineState];
    _lastUniformOffset = 0;

    if (_renderEncoder == nil) {
        return;
    }
    MNVGcall* call = &renderData->calls[0];
    for (int i = renderData->ncalls; i--; ++call) {
        MNVGblend* blend = &call->blendFunc;
        [self updateRenderPipelineStatesForBlend:blend
                                     pixelFormat:colorTexture.pixelFormat];
        if (call->type == MNVG_FILL)
            [self fill:call];
        else if (call->type == MNVG_CONVEXFILL)
            [self convexFill:call];
        else if (call->type == MNVG_STROKE)
            [self stroke:call];
        else if (call->type == MNVG_TRIANGLES)
            [self triangles:call];
    }

    [_renderEncoder endEncoding];
    _renderEncoder = nil;

    if (drawable && !_metalLayer.presentsWithTransaction) {
        [_buffers.commandBuffer presentDrawable:drawable];
    }

    [_buffers.commandBuffer commit];

    if (drawable && _metalLayer.presentsWithTransaction) {
        [_buffers.commandBuffer waitUntilScheduled];
        [drawable present];
    }

    _lastBoundTexture = -1;
}

- (int)renderGetTextureSizeForImage:(int)image
                              width:(int*)width
                             height:(int*)height {
    MNVGtexture* tex = [self findTexture:image];
    if (tex == nil) return 0;
    *width = (int)tex->tex.width;
    *height = (int)tex->tex.height;
    return 1;
}

- (void)renderStrokeWithPaint:(NVGpaint*)paint
           compositeOperation:(NVGcompositeOperationState)compositeOperation
                      scissor:(NVGscissor*)scissor
                       fringe:(float)fringe
                  strokeWidth:(float)strokeWidth
                    lineStyle: (int)lineStyle
                   lineLength: (float)lineLength
                        paths:(const NVGpath*)paths
                       npaths:(int)npaths
{
    MNVGcall* call = [self allocCall];

    if (call == NULL) return;

    MNVGrenderData* renderData = _buffers.renderData;
    call->type = MNVG_STROKE;
    call->image = paint->image;
    call->blendFunc = [self blendCompositeOperation:compositeOperation];

    // Allocate vertices for all the paths.
    int strokeCount = 0;
    int maxverts = mtlnvg__maxVertCount(paths, npaths, NULL, &strokeCount);
    int offset = [self allocVerts:maxverts];
    if (offset == -1) goto error;

    call->strokeOffset = offset + 1;
    call->strokeCount = strokeCount - 2;
    NVGvertex* strokeVert = renderData->verts + offset;

    NVGpath* path = (NVGpath*)&paths[0];
    int lineReversed = 0;
    for (int i = npaths; i--; ++path) {
        if (path->nstroke > 0) {
            lineReversed = path->reversed;
            memcpy(strokeVert, path->stroke, sizeof(NVGvertex));
            ++strokeVert;
            memcpy(strokeVert, path->stroke, sizeof(NVGvertex) * path->nstroke);
            strokeVert += path->nstroke;
            memcpy(strokeVert, path->stroke + path->nstroke - 1, sizeof(NVGvertex));
            ++strokeVert;
        }
    }

    if (false /*_flags & NVG_STENCIL_STROKES*/) {
        // Fill shader
        call->uniformOffset = [self allocFragUniforms:2];
        if (call->uniformOffset == -1) goto error;
        [self convertPaintForFrag:[self fragUniformAtIndex:call->uniformOffset]
                            paint:paint
                          scissor:scissor
                            width:strokeWidth
                           fringe:fringe
                        lineStyle:lineStyle
                       lineLength:lineLength
                     lineReversed:lineReversed];
        MNVGfragUniforms* frag = [self
                                  fragUniformAtIndex:call->uniformOffset + _fragSize];
        [self convertPaintForFrag:frag
                            paint:paint
                          scissor:scissor
                            width:strokeWidth
                           fringe:fringe
                        lineStyle:lineStyle
                       lineLength:lineLength
                     lineReversed:lineReversed];
    } else {
        // Fill shader
        call->uniformOffset = [self allocFragUniforms:1];
        if (call->uniformOffset == -1) goto error;
        [self convertPaintForFrag:[self fragUniformAtIndex:call->uniformOffset]
                            paint:paint
                          scissor:scissor
                            width:strokeWidth
                           fringe:fringe
                        lineStyle:lineStyle
                       lineLength:lineLength
                     lineReversed:lineReversed];
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (renderData->ncalls > 0) renderData->ncalls--;
}

- (void)renderTrianglesWithPaint:(NVGpaint*) paint
              compositeOperation:(NVGcompositeOperationState)compositeOperation
                         scissor:(NVGscissor*)scissor
                           verts:(const NVGvertex*)verts
                          nverts:(int)nverts
                          fringe:(float)fringe
                            text:(int)text {
    MNVGcall* call = [self allocCall];
    MNVGfragUniforms* frag;

    if (call == NULL) return;

    MNVGrenderData* renderData = _buffers.renderData;
    call->type = MNVG_TRIANGLES;
    call->image = paint->image;
    call->blendFunc = [self blendCompositeOperation:compositeOperation];

    // Allocate vertices for all the paths.
    call->triangleOffset = [self allocVerts:nverts];
    if (call->triangleOffset == -1) goto error;
    call->triangleCount = nverts;

    memcpy(&renderData->verts[call->triangleOffset], verts,
           sizeof(NVGvertex) * nverts);

    // Fill shader
    call->uniformOffset = [self allocFragUniforms:1];
    if (call->uniformOffset == -1) goto error;
    frag = [self fragUniformAtIndex:call->uniformOffset];
    [self convertPaintForFrag:frag
                        paint:paint
                      scissor:scissor
                        width:1.0f
                       fringe:fringe
                    lineStyle:NVG_LINE_SOLID
                   lineLength:0.0f
                 lineReversed:0];

    if(text) {
        frag->stateData = mtlnvg_packStateDataUniform(PACK_TYPE, MNVG_SHADER_IMG) | mtlnvg_packStateDataUniform(PACK_TEX_TYPE, 2);
    }

    return;

error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (renderData->ncalls > 0) renderData->ncalls--;
}

- (int)renderUpdateTextureWithImage:(int)image
                                  x:(int)x
                                  y:(int)y
                              width:(int)width
                             height:(int)height
                               data:(const unsigned char*)data {
    MNVGtexture* tex = [self findTexture:image];

    if (tex == nil) return 0;

    unsigned char* bytes;
    NSUInteger bytesPerRow;
    if (tex->type == NVG_TEXTURE_RGBA) {
        bytesPerRow = tex->tex.width * 4;
        bytes = (unsigned char*)data + y * bytesPerRow + x * 4;
    } else {
        bytesPerRow = tex->tex.width;
        bytes = (unsigned char*)data + y * bytesPerRow + x;
    }

#if TARGET_OS_SIMULATOR
    const NSUInteger kBufferSize = bytesPerRow * height;
    id<MTLBuffer> buffer = [_metalLayer.device
                            newBufferWithLength:kBufferSize
                            options:MTLResourceStorageModeShared];
    memcpy([buffer contents], bytes, kBufferSize);

    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    id<MTLBlitCommandEncoder> blitCommandEncoder = [commandBuffer
                                                    blitCommandEncoder];
    [blitCommandEncoder copyFromBuffer:buffer
                          sourceOffset:0
                     sourceBytesPerRow:bytesPerRow
                   sourceBytesPerImage:kBufferSize
                            sourceSize:MTLSizeMake(width, height, 1)
                             toTexture:tex->tex
                      destinationSlice:0
                      destinationLevel:0
                     destinationOrigin:MTLOriginMake(x, y, 0)];

    [blitCommandEncoder endEncoding];
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
#else
    id<MTLTexture> texture = tex->tex;
    [texture replaceRegion:MTLRegionMake2D(x, y, width, height)
               mipmapLevel:0
                 withBytes:bytes
               bytesPerRow:bytesPerRow];
#endif

    return 1;
}

- (void)renderViewportWithWidth:(float)width
                         height:(float)height
               devicePixelRatio:(float)devicePixelRatio {
    _viewPortSize = (vector_uint2){width * devicePixelRatio,
        height * devicePixelRatio};

    dispatch_semaphore_wait(_semaphore, DISPATCH_TIME_FOREVER);
    for (MNVGbuffers* buffers in _cbuffers) {
        if (!buffers.isBusy) {
            buffers.isBusy = YES;
            _buffers = buffers;
            break;
        }
    }

    // Initializes view size buffer for vertex function.
    if (_buffers.viewSizeBuffer == nil) {
        _buffers.viewSizeBuffer = [_metalLayer.device
                                   newBufferWithLength:sizeof(vector_float2)
                                   options:kMetalBufferOptions];
    }
    float* viewSize = (float*)[_buffers.viewSizeBuffer contents];
    viewSize[0] = width;
    viewSize[1] = height;
}

- (void)setUniforms:(int)uniformOffset image:(int)image {
    if(_lastUniformOffset != uniformOffset) {
        [_renderEncoder setFragmentBufferOffset:uniformOffset atIndex:0];
        _lastUniformOffset = uniformOffset;
    }

    if ( _lastBoundTexture != image) {
        MNVGtexture* tex = image ? [self findTexture:image] : nil;
        [_renderEncoder setFragmentTexture:(tex != nil ? tex->tex : _pseudoTexture) atIndex:0];
        [_renderEncoder setFragmentSamplerState:(tex != nil ? tex->sampler : _pseudoSampler) atIndex:0];
        _lastBoundTexture = image;
    }
}

- (void)stroke:(MNVGcall*)call {
    if (call->strokeCount <= 0) {
        return;
    }

    if (false /*_flags & NVG_STENCIL_STROKES*/) {
        // Fills the stroke base without overlap.
        [self setUniforms:(call->uniformOffset + _fragSize) image:call->image];
        [_renderEncoder setDepthStencilState:_strokeShapeStencilState];
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];

        // Draws anti-aliased fragments.
        [self setUniforms:call->uniformOffset image:call->image];
        [_renderEncoder setDepthStencilState:_strokeAntiAliasStencilState];
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];

        // Clears stencil buffer.
        [_renderEncoder setDepthStencilState:_strokeClearStencilState];
        [_renderEncoder setRenderPipelineState:_stencilOnlyPipelineState];
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];
        [_renderEncoder setDepthStencilState:_defaultStencilState];
        [_renderEncoder setRenderPipelineState:_pipelineState];
    } else {
        // Draws strokes.
        [self setUniforms:call->uniformOffset image:call->image];
        [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                           vertexStart:call->strokeOffset
                           vertexCount:call->strokeCount];
    }
}

- (void)triangles:(MNVGcall*)call {
    [self setUniforms:call->uniformOffset image:call->image];
    [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                       vertexStart:call->triangleOffset
                       vertexCount:call->triangleCount];
}

- (void)updateRenderPipelineStatesForBlend:(MNVGblend*)blend
                               pixelFormat:(MTLPixelFormat)pixelFormat
{
    if (_pipelineState != nil &&
        _stencilOnlyPipelineState != nil &&
        _piplelinePixelFormat == pixelFormat &&
        _blendFunc->srcRGB == blend->srcRGB &&
        _blendFunc->dstRGB == blend->dstRGB &&
        _blendFunc->srcAlpha == blend->srcAlpha &&
        _blendFunc->dstAlpha == blend->dstAlpha) {
        return;
    }

    MTLRenderPipelineDescriptor* pipelineStateDescriptor = \
    [MTLRenderPipelineDescriptor new];

    MTLRenderPipelineColorAttachmentDescriptor* colorAttachmentDescriptor = \
    pipelineStateDescriptor.colorAttachments[0];
    colorAttachmentDescriptor.pixelFormat = pixelFormat;
    pipelineStateDescriptor.stencilAttachmentPixelFormat = kStencilFormat;
    pipelineStateDescriptor.fragmentFunction = _fragmentFunction;
    pipelineStateDescriptor.vertexFunction = _vertexFunction;
    pipelineStateDescriptor.vertexDescriptor = _vertexDescriptor;

    // Sets blending states.
    colorAttachmentDescriptor.blendingEnabled = YES;
    colorAttachmentDescriptor.sourceRGBBlendFactor = blend->srcRGB;
    colorAttachmentDescriptor.sourceAlphaBlendFactor = blend->srcAlpha;
    colorAttachmentDescriptor.destinationRGBBlendFactor = blend->dstRGB;
    colorAttachmentDescriptor.destinationAlphaBlendFactor = blend->dstAlpha;
    _blendFunc->srcRGB = blend->srcRGB;
    _blendFunc->dstRGB = blend->dstRGB;
    _blendFunc->srcAlpha = blend->srcAlpha;
    _blendFunc->dstAlpha = blend->dstAlpha;

    NSError* error;
    _pipelineState = [_metalLayer.device
                      newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                      error:&error];
    [self checkError:error withMessage:"init pipeline state"];

    pipelineStateDescriptor.fragmentFunction = nil;
    colorAttachmentDescriptor.writeMask = MTLColorWriteMaskNone;
    _stencilOnlyPipelineState = [_metalLayer.device
                                 newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                 error:&error];
    [self checkError:error withMessage:"init pipeline stencil only state"];

    _piplelinePixelFormat = pixelFormat;
    [_renderEncoder setRenderPipelineState:_pipelineState];
}

// Re-creates stencil texture whenever the specified size is bigger.
- (void)updateStencilTextureToSize:(vector_uint2*)size {
    if (_buffers.stencilTexture != nil &&
        (_buffers.stencilTexture.width < size->x ||
         _buffers.stencilTexture.height < size->y)) {
        _buffers.stencilTexture = nil;
    }
    if (_buffers.stencilTexture == nil) {
        MTLTextureDescriptor* stencilTextureDescriptor = [MTLTextureDescriptor
                                                          texture2DDescriptorWithPixelFormat:kStencilFormat
                                                          width:size->x
                                                          height:size->y
                                                          mipmapped:NO];
        stencilTextureDescriptor.usage = MTLTextureUsageRenderTarget;
#if TARGET_OS_OSX || TARGET_OS_SIMULATOR || TARGET_OS_MACCATALYST
        stencilTextureDescriptor.storageMode = MTLStorageModePrivate;
#endif  // TARGET_OS_OSX || TARGET_OS_SIMULATOR || TARGET_OS_MACCATALYST
        _buffers.stencilTexture = [_metalLayer.device
                                   newTextureWithDescriptor:stencilTextureDescriptor];
    }
}

@end

@implementation MNVGtexture
@end
