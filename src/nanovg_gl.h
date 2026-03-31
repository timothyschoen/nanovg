//
// Copyright (c) 2009-2013 Mikko Mononen memon@inside.org
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
//
#ifndef NANOVG_GL_H
#define NANOVG_GL_H

#ifdef __cplusplus
extern "C" {
#endif

#define NVG_DEBUG 0

enum PackType {
    PACK_LINE_STYLE,
    PACK_TEX_TYPE,
    PACK_REVERSE,
    PACK_FLAG_TYPE,
    PACK_OBJECT_STYLE
};

// This allows us to place 4 int values into a single int to save space and improve speed for CPU->GPU upload
// and openGL checking of uniform state as it only needs to do this for 1 int uniform as opposed to 4
int glnvg__packStateDataUniform(PackType packType, int value) {
    switch (packType) {
        case PACK_OBJECT_STYLE:
            return (value & 0x01) << 11;
        case PACK_FLAG_TYPE:
            return (value & 0x03) << 9;
        case PACK_LINE_STYLE:
            return (value & 0x03) << 7;
        case PACK_TEX_TYPE:
            return (value & 0x03) << 5;
        case PACK_REVERSE:
            return value & 0x01;
        default:
            return 0;
    }
}

#if defined NANOVG_GL3_IMPLEMENTATION
#define NANOVG_GL3 1
#define NANOVG_GL_IMPLEMENTATION 1
NVGcontext* nvgCreateGL3(int flags);
void nvgDeleteGL3(NVGcontext* ctx);
#else
#define NANOVG_GLES3 1
#define NANOVG_GL_IMPLEMENTATION 1
NVGcontext* nvgCreateGLES3(int flags);
void nvgDeleteGLES3(NVGcontext* ctx);
#endif

void nvglClearWithColor(NVGcolor color);


// These are additional flags on top of NVGimageFlags.
enum NVGimageFlagsGL {
    NVG_IMAGE_NODELETE            = 1<<16,    // Do not delete GL texture handle.
};

#ifdef __cplusplus
}
#endif

#endif /* NANOVG_GL_H */

#ifdef NANOVG_GL_IMPLEMENTATION

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "nanovg.h"

enum GLNVGuniformLoc {
    GLNVG_LOC_VIEWSIZE,
    GLNVG_LOC_TEX,
    GLNVG_LOC_FRAG,
    GLNVG_MAX_LOCS
};

enum GLNVGuniformBindings {
    GLNVG_FRAG_BINDING = 0,
};

struct GLNVGshader {
    GLuint prog;
    GLuint frag;
    GLuint vert;
    GLint loc[GLNVG_MAX_LOCS];
};
typedef struct GLNVGshader GLNVGshader;

struct GLNVGtexture {
    int id;
    GLuint tex;
    int width, height;
    int type;
    int flags;
    int valid;
};
typedef struct GLNVGtexture GLNVGtexture;


const GLenum GLNVGBlendFactors[12] = {
    0,
    GL_ZERO,
    GL_ONE,
    GL_SRC_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_ALPHA,
    GL_SRC_ALPHA_SATURATE
};

struct GLNVGblend
{
    GLenum srcRGB;
    GLenum dstRGB;
    GLenum srcAlpha;
    GLenum dstAlpha;
};
typedef struct GLNVGblend GLNVGblend;

enum GLNVGcallType {
    GLNVG_NONE = 0,
    GLNVG_FILL,
    GLNVG_CONVEXFILL,
    GLNVG_STROKE,
    GLNVG_TRIANGLES,
};

struct GLNVGcall {
    int type;
    int image;
    int pathOffset;
    int pathCount;
    int triangleOffset;
    int triangleCount;
    int uniformOffset;
    GLNVGblend blendFunc;
};
typedef struct GLNVGcall GLNVGcall;

struct GLNVGpath {
    int fillOffset;
    int fillCount;
    int strokeOffset;
    int strokeCount;
};
typedef struct GLNVGpath GLNVGpath;

struct GLNVGfragUniforms {
    float scissorMat[8];
    float paintMat[8];
    uint32_t innerCol;
    uint32_t outerCol;
    uint32_t dashCol;
    int stateData;
    float scissorExt[2];
    float extent[2];
    float radius;
    float feather;
    float strokeMult;
    float scissorRadius;
    float lineLength;
    float offset;
    int type;
};
typedef struct GLNVGfragUniforms GLNVGfragUniforms;

struct GLNVGcontext {
    GLNVGshader shader;
    GLNVGtexture* textures;
    float view[2];
    float devicePixelRatio;
    int ntextures;
    int ctextures;
    int textureId;
    GLuint vertBuf;
    GLuint vertArr;
    GLuint fragBuf;
    int fragSize;
    int flags;
    
    // Per frame buffers
    GLNVGcall* calls;
    int ccalls;
    int ncalls;
    GLNVGpath* paths;
    int cpaths;
    int npaths;
    struct NVGvertex* verts;
    int cverts;
    int nverts;
    unsigned char* uniforms;
    int cuniforms;
    int nuniforms;
    
    // cached state
    GLuint boundTexture;
    GLuint stencilMask;
    GLenum stencilFunc;
    GLint stencilFuncRef;
    GLuint stencilFuncMask;
    GLNVGblend blendFunc;
    
    int current_uniform_size;
    int current_vert_array_size;
    
    int dummyTex;
};
typedef struct GLNVGcontext GLNVGcontext;

static int glnvg__maxi(int a, int b) { return a > b ? a : b; }

static void glnvg__bindTexture(GLNVGcontext* gl, GLuint tex)
{
    if (gl->boundTexture != tex) {
        gl->boundTexture = tex;
        glBindTexture(GL_TEXTURE_2D, tex);
    }
}

static void glnvg__stencilMask(GLNVGcontext* gl, GLuint mask)
{
    if (gl->stencilMask != mask) {
        gl->stencilMask = mask;
        glStencilMask(mask);
    }
}

static void glnvg__stencilFunc(GLNVGcontext* gl, GLenum func, GLint ref, GLuint mask)
{
    if ((gl->stencilFunc != func) ||
        (gl->stencilFuncRef != ref) ||
        (gl->stencilFuncMask != mask)) {
        
        gl->stencilFunc = func;
        gl->stencilFuncRef = ref;
        gl->stencilFuncMask = mask;
        glStencilFunc(func, ref, mask);
    }
}
static void glnvg__blendFuncSeparate(GLNVGcontext* gl, const GLNVGblend* blend)
{
    if ((gl->blendFunc.srcRGB != blend->srcRGB) ||
        (gl->blendFunc.dstRGB != blend->dstRGB) ||
        (gl->blendFunc.srcAlpha != blend->srcAlpha) ||
        (gl->blendFunc.dstAlpha != blend->dstAlpha)) {
        
        gl->blendFunc = *blend;
        glBlendFuncSeparate(blend->srcRGB, blend->dstRGB, blend->srcAlpha,blend->dstAlpha);
    }
}

static GLNVGtexture* glnvg__allocTexture(GLNVGcontext* gl)
{
    GLNVGtexture* tex = NULL;
    int i;
    for (i = 0; i < gl->ntextures; i++) {
        if (gl->textures[i].valid == 0) {
            tex = &gl->textures[i];
            break;
        }
    }
    if (tex == NULL) {
        if (gl->ntextures+1 > gl->ctextures) {
            GLNVGtexture* textures;
            int ctextures = glnvg__maxi(gl->ntextures+1, 4) +  gl->ctextures/2; // 1.5x Overallocate
            textures = (GLNVGtexture*)realloc(gl->textures, sizeof(GLNVGtexture)*ctextures);
            if (textures == NULL) return NULL;
            gl->textures = textures;
            gl->ctextures = ctextures;
        }
        tex = &gl->textures[gl->ntextures++];
        memset(tex, 0, sizeof(*tex));
        tex->id = ++gl->textureId;
    }
    tex->valid = 1;
    return tex;
}

static GLNVGtexture* glnvg__findTexture(GLNVGcontext* gl, int id)
{
    if(id <= 0) return NULL;
    return &gl->textures[id - 1];
}

static int glnvg__deleteTexture(GLNVGcontext* gl, int id)
{
    if(id <= 0) return 0;
    GLNVGtexture* texture = &gl->textures[id-1];
    
    if (texture->tex != 0 && (texture->flags & NVG_IMAGE_NODELETE) == 0)
        glDeleteTextures(1, &texture->tex);
    
    texture->valid = 0;
    return 1;
}

static void glnvg__dumpShaderError(GLuint shader, const char* name, const char* type)
{
    GLchar str[512+1];
    GLsizei len = 0;
    glGetShaderInfoLog(shader, 512, &len, str);
    if (len > 512) len = 512;
    str[len] = '\0';
    printf("Shader %s/%s error:\n%s\n", name, type, str);
}

static void glnvg__dumpProgramError(GLuint prog, const char* name)
{
    GLchar str[512+1];
    GLsizei len = 0;
    glGetProgramInfoLog(prog, 512, &len, str);
    if (len > 512) len = 512;
    str[len] = '\0';
    printf("Program %s error:\n%s\n", name, str);
}

static void glnvg__checkError(GLNVGcontext* gl, const char* str)
{
    GLenum err;
    if (!NVG_DEBUG) return;
    err = glGetError();
    if (err != GL_NO_ERROR) {
        printf("Error %08x after %s\n", err, str);
        return;
    }
}

static int glnvg__createShader(GLNVGshader* shader, const char* header, const char* vshader, const char* fshader)
{
    GLint status;
    GLuint prog, vert, frag;
    const char* str[2];
    str[0] = header;
    
    memset(shader, 0, sizeof(*shader));
    
    prog = glCreateProgram();
    vert = glCreateShader(GL_VERTEX_SHADER);
    frag = glCreateShader(GL_FRAGMENT_SHADER);
    str[1] = vshader;
    glShaderSource(vert, 2, str, 0);
    str[1] = fshader;
    glShaderSource(frag, 2, str, 0);
    
    glCompileShader(vert);
    glGetShaderiv(vert, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpShaderError(vert, "shader", "vert");
        return 0;
    }
    
    glCompileShader(frag);
    glGetShaderiv(frag, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpShaderError(frag, "shader", "frag");
        return 0;
    }
    
    glAttachShader(prog, vert);
    glAttachShader(prog, frag);
    
    glBindAttribLocation(prog, 0, "vertex");
    glBindAttribLocation(prog, 1, "tcoord");
    
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &status);
    if (status != GL_TRUE) {
        glnvg__dumpProgramError(prog, "shader");
        return 0;
    }
    
    shader->prog = prog;
    shader->vert = vert;
    shader->frag = frag;
    
    return 1;
}

static void glnvg__deleteShader(GLNVGshader* shader)
{
    if (shader->prog != 0)
        glDeleteProgram(shader->prog);
    if (shader->vert != 0)
        glDeleteShader(shader->vert);
    if (shader->frag != 0)
        glDeleteShader(shader->frag);
}

static void glnvg__getUniforms(GLNVGshader* shader)
{
    shader->loc[GLNVG_LOC_VIEWSIZE] = glGetUniformLocation(shader->prog, "viewSize");
    shader->loc[GLNVG_LOC_TEX] = glGetUniformLocation(shader->prog, "tex");
    shader->loc[GLNVG_LOC_FRAG] = glGetUniformBlockIndex(shader->prog, "frag");
}

static void glnvg__updateTexPixelStoreiVals(int alignement, int x, int y, int w)
{
    glPixelStorei(GL_UNPACK_ALIGNMENT, alignement);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, w);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, x);
    glPixelStorei(GL_UNPACK_SKIP_ROWS, y);
}


int nvg__renderCreateTexture(void* uptr, int type, int w, int h, int imageFlags, const unsigned char* data)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__allocTexture(gl);
    
    if (tex == NULL) return 0;
    
    glGenTextures(1, &tex->tex);
    tex->width = w;
    tex->height = h;
    tex->type = type;
    tex->flags = imageFlags;
    glnvg__bindTexture(gl, tex->tex);
    
    glnvg__updateTexPixelStoreiVals(4, 0, 0, 0);
    
    if (type == NVG_TEXTURE_ARGB)
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_BGRA, GL_UNSIGNED_BYTE, data);
    else if(type == NVG_TEXTURE_ARGB_SRGB)
        glTexImage2D(GL_TEXTURE_2D, 0, GL_SRGB8_ALPHA8, w, h, 0, GL_BGRA, GL_UNSIGNED_BYTE, data);
    else
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, w, h, 0, GL_RED, GL_UNSIGNED_BYTE, data);
    
    if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS) {
        if (imageFlags & NVG_IMAGE_NEAREST) {
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
        } else {
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        }
    } else {
        if (imageFlags & NVG_IMAGE_NEAREST) {
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        } else {
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        }
    }
    
    if (imageFlags & NVG_IMAGE_NEAREST) {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    } else {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    }
    
    if (imageFlags & NVG_IMAGE_REPEATX)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    else
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    
    if (imageFlags & NVG_IMAGE_REPEATY)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    else
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    
    // The new way to build mipmaps on GLES and GL3
    if (imageFlags & NVG_IMAGE_GENERATE_MIPMAPS) {
        glGenerateMipmap(GL_TEXTURE_2D);
    }
    
    glnvg__checkError(gl, "create tex");
    glnvg__bindTexture(gl, 0);
    
    return tex->id;
}


int nvg__renderDeleteTexture(void* uptr, int image)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    return glnvg__deleteTexture(gl, image);
}

int nvg__renderUpdateTexture(void* uptr, int image, int x, int y, int w, int h, const unsigned char* data)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);
    
    if (tex == NULL) return 0;
    glnvg__bindTexture(gl, tex->tex);
    
    glnvg__updateTexPixelStoreiVals(4, x, y, tex->width);
    
    if (tex->type == NVG_TEXTURE_ARGB || tex->type == NVG_TEXTURE_ARGB_SRGB)
        glTexSubImage2D(GL_TEXTURE_2D, 0, x,y, w,h, GL_BGRA, GL_UNSIGNED_BYTE, data);
    else
        glTexSubImage2D(GL_TEXTURE_2D, 0, x,y, w,h, GL_RED, GL_UNSIGNED_BYTE, data);
    
    glnvg__bindTexture(gl, 0);
    
    return 1;
}

int nvg__renderGetTextureSize(void* uptr, int image, int* w, int* h)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);
    if (tex == NULL) return 0;
    *w = tex->width;
    *h = tex->height;
    return 1;
}

int nvg_checkGLVersion()
{
    GLint majorVersion = 0;
    glGetIntegerv(GL_MAJOR_VERSION, &majorVersion);
    GLenum error = glGetError();
    
    if (error != GL_NO_ERROR || majorVersion < 3) {
        // Fall back to parsing GL_VERSION string for older OpenGL
        char const* version = (char const*)glGetString(GL_VERSION);
        if (version == NULL)
            return 0; // Failed
        
        // Parse version string (format: "X.Y" or "X.Y.Z")
        majorVersion = version[0] - '0';
        
        if (majorVersion < 3)
            return 0; // OpenGL 3.0+ required
    }
    
    return 1;
}

int nvg__renderCreate(void* uptr)
{
    if (!nvg_checkGLVersion()) return 0;
    
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    int align = 4;
    
    // Construct the shader header with correct defines
    std::ostringstream shaderHeader;
    
#if defined NANOVG_GL3
    shaderHeader << "#version 150 core\n";
#elif defined NANOVG_GLES3
    shaderHeader << "#version 300 es\n";
#endif
    
    shaderHeader << "#define NANOVG_GL3 1\n"
    << "#define NSVG_SHADER_FILLGRAD               " << PAINT_TYPE_FILLGRAD << "\n"
    << "#define NSVG_SHADER_FILLIMG                " << PAINT_TYPE_FILLIMG << "\n"
    << "#define NSVG_SHADER_FILLCOLOR              " << PAINT_TYPE_FILLCOLOR << "\n"
    << "#define NSVG_SHADER_TEXT                   " << PAINT_TYPE_TEXT << "\n"
    << "#define NSVG_SHADER_FAST_ROUNDEDRECT       " << PAINT_TYPE_FAST_ROUNDEDRECT << "\n"
    << "#define NSVG_SHADER_OBJECT_RECT            " << PAINT_TYPE_OBJECT_RECT << "\n"
    << "#define NSVG_SMOOTH_GLOW                   " << PAINT_TYPE_SMOOTH_GLOW << "\n"
    << "#define NSVG_DOUBLE_STROKE                 " << PAINT_TYPE_DOUBLE_STROKE << "\n"
    << "#define NSVG_DOUBLE_STROKE_GRAD            " << PAINT_TYPE_DOUBLE_STROKE_GRAD << "\n"
    << "#define NSVG_DOUBLE_STROKE_ACTIVITY        " << PAINT_TYPE_DOUBLE_STROKE_ACTIVITY << "\n"
    << "#define NSVG_DOUBLE_STROKE_GRAD_ACTIVITY   " << PAINT_TYPE_DOUBLE_STROKE_GRAD_ACTIVITY << "\n"
    << "#define NSVG_SHADER_FILLIMG_ALPHA          " << PAINT_TYPE_FILLIMG_ALPHA << "\n"
    
    << "#define NSVG_TEXTURE_ALPHA                 " << NVG_TEXTURE_ALPHA << "\n"
    << "#define NSVG_TEXTURE_ARGB                  " << NVG_TEXTURE_ARGB << "\n"
    << "#define NSVG_TEXTURE_ARGB_SRGB             " << NVG_TEXTURE_ARGB_SRGB << "\n"
    << "\n";
    
    static char const* fillVertShader = R"(
        uniform vec2 viewSize;
        in vec2 vertex;
        in vec4 tcoord;
        out vec2 ftcoord;
        out vec2 fpos;
        smooth out vec2 uv;
    
        void main(void) {
            ftcoord = tcoord.xy * 2.0f;
            uv = tcoord.zw;
            fpos = vertex;
            gl_Position = vec4(2.0f*vertex.x/viewSize.x - 1.0f, 1.0f - 2.0f*vertex.y/viewSize.y, 0.f, 1.f);
        }
    )";
    
    std::stringstream fillFragShader;
    
    fillFragShader << R"(
        #ifdef GL_ES
        precision highp float;
        #endif
        
        // Has easier alignment than mat3x2 or float[6]
        struct affine_transform
        {
            float t0, t1, t2, t3, t4, t5;
        };
        
        layout(std140) uniform frag {
            affine_transform scissorMat;
            affine_transform paintMat;
            int innerCol;
            int outerCol;
            int dashCol;
            int stateData;
            vec2 scissorExt;
            vec2 extent;
            float radius;
            float feather;
            float strokeMult;
            float scissorRadius;
            float lineLength;
            float offset;
            int type;
        };
        uniform sampler2D tex;
        in vec2 ftcoord;
        in vec2 fpos;
        smooth in vec2 uv;
        out vec4 outColor;

        vec4 getRawColour(int bgra){
            return vec4(
                (bgra >> 16) & 0xFF,  // R
                (bgra >> 8)  & 0xFF,  // G
                 bgra        & 0xFF,  // B
                (bgra >> 24) & 0xFF   // A
            ) / 255.0;
        }

        vec4 convertColour(int bgra){
            vec4 col = getRawColour(bgra);
            return vec4(col.rgb * col.a, col.a);
        }

        float sdroundrect(vec2 pt, vec2 ext, float rad) {
            vec2 ext2 = ext - vec2(rad,rad);
            vec2 d = abs(pt) - ext2;
            return min(max(d.x,d.y),0.0f) + length(max(d,0.0f)) - rad;
        }
        vec2 rotatePoint(vec2 p, float angle) {
            float cosAngle = cos(angle);
            float sinAngle = sin(angle);
            mat2 rotationMatrix = mat2(
                cosAngle, -sinAngle,
                sinAngle,  cosAngle
            );
            return rotationMatrix * p;
        }
        float sdSegment(vec2 p, vec2 a, vec2 b ) {
            vec2 pa = p-a, ba = b-a;
            float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0f, 1.0f );
            return length( pa - ba*h );
        }
        float inverseLerp(float a, float b, float value) {
            return (value - a) / (b - a);
        }
        mat3 transformInverse(const affine_transform t) {
            float det = t.t0 * t.t3 - t.t1 * t.t2;
            if(det == 0.0f) return mat3(0.0f);
        
            float invdet = 1.0f / det;
        
            return mat3(
                vec3(t.t3 * invdet, -t.t1 * invdet, 0.0f),
                vec3(-t.t2 * invdet, t.t0 * invdet, 0.0f),
                vec3((t.t2 * t.t5 - t.t3 * t.t4) * invdet, (t.t1 * t.t4 - t.t0 * t.t5) * invdet, 1.0f));
        }
        vec4 normalBlend(vec4 src, vec4 dst) {
            float finalAlpha = src.a + dst.a * (1.0 - src.a);
            return vec4((src.rgb * src.a + dst.rgb * dst.a * (1.0 - src.a)) / finalAlpha, finalAlpha);
        }
        vec4 linearToSrgb(vec4 col)
        {
            vec3 c = col.rgb;
            vec3 lo = c * 12.92;
            vec3 hi = 1.055 * pow(c, vec3(1.0/2.4)) - 0.055;
        
            bvec3 mask = lessThanEqual(c, vec3(0.0031308));
            return vec4(mix(hi, lo, mask), col.a);  // mix(a,b,mask) selects b where mask is true
        }
        float sigmoid(float t) {
            return 1.0 / (1.0 + exp(-t));
        }
        // Calculate scissor path with rounded corners
        float scissorMask(vec2 p, float rad) {
            vec2 sc = (abs((transformInverse(scissorMat) * vec3(p,1.0f)).xy));
            float sc2 = sdroundrect(sc, scissorExt, rad) - 0.04f;
            float sc3 = fwidth(sc2) * 0.5;
            return clamp(inverseLerp(sc3, -sc3, sc2), 0.0f, 1.0f);
        }
        float glow(vec2 uv){
            return smoothstep(0.0f, 1.0f, 1.0f - 2.0f * abs(uv.x));
        }
        float circleDist(vec2 p, vec2 center, float d) {
             return distance(center, p) - d;
        }
        float dashed(vec2 uv, float rad, float thickness, float featherVal){
            float fy = mod(uv.y, rad);
            float radThick = rad * .25f;
            float seg = sdSegment(vec2(uv.x, fy), vec2(0.0f, radThick + thickness), vec2(0.0f, (rad * 0.5f) + radThick - thickness)) - thickness;
            float delta = fwidth(seg) * 0.5f;
            float aa = delta;
            float w = clamp(inverseLerp(aa, -aa, seg), 0.0f, 1.0f);
            return w;
        }
        float dotted(vec2 uv){
            float fy = 4.0f * fract(uv.y / (4.0)) - 0.5f;
            return smoothstep(0.0f, 1.0f, 6.0f * (0.25f - (uv.x * uv.x  + fy * fy)));
        }
        // Stroke - from [0..1] to clipped pyramid, where the slope is 1px.
        float strokeMask(int lineStyle) {
            float mask = min(1.0f, (1.0f-abs(ftcoord.x*2.0f-1.0f))*strokeMult) * min(1.0f, ftcoord.y);
            if(lineStyle == 2) mask*=dashed(vec2(uv.x, uv.y * lineLength - offset), radius, 0.45f, 0.0f);
            if(lineStyle == 3) mask*=dotted(uv);
            if(lineStyle == 4) mask*=glow(uv);
            return mask;
        }
        int getLineStyle(){
            return (stateData >> 7) & 0x03;     // 2 bits
        }
        int getTexType(){
            return (stateData >> 5) & 0x03;     // 3 bits (0,1,2,3,4)
        }
        bool getReverse(){
            return bool(stateData & 0x01);      // 1 bit
        }
        
        vec4 sampleLevel0( sampler2D tex, vec2 uv )
        {
            return textureLod( tex, uv, 0.0 );
        }
        
        float mitchell(float x, float B, float C) {
            x = abs(x);
            if (x < 1.0) {
                return ((12.0 - 9.0*B - 6.0*C) * x*x*x +
                        (-18.0 + 12.0*B + 6.0*C) * x*x +
                        (6.0 - 2.0*B)) / 6.0;
            } else if (x < 2.0) {
                return ((-B - 6.0*C) * x*x*x +
                        (6.0*B + 30.0*C) * x*x +
                        (-12.0*B - 48.0*C) * x +
                        (8.0*B + 24.0*C)) / 6.0;
            }
            return 0.0;
        }
        
        vec4 textureMitchellNetravali(sampler2D tex, vec2 uv) {
            const float B = 1.0/2.0;
            const float C = 1.0/6.0;
        
            vec2 texSize = vec2(textureSize(tex, 0));
            vec2 samplePos = uv * texSize;
            vec2 texPos1 = floor(samplePos - 0.5) + 0.5;
            vec2 f = samplePos - texPos1;
        
            // Calculate weights for 4 taps in each direction
            float wx[4], wy[4];
            for (int i = 0; i < 4; i++) {
                wx[i] = mitchell(f.x - float(i - 1), B, C);
                wy[i] = mitchell(f.y - float(i - 1), B, C);
            }
        
            // Combine middle weights for bilinear optimization
            float wx12 = wx[1] + wx[2];
            float wy12 = wy[1] + wy[2];
            vec2 offset12 = vec2(wx[2] / (wx12 + 0.0001), wy[2] / (wy12 + 0.0001));
        
            // Calculate sample positions
            vec2 texPos0 = texPos1 - 1.0;
            vec2 texPos3 = texPos1 + 2.0;
            vec2 texPos12 = texPos1 + offset12;
        
            texPos0 /= texSize;
            texPos3 /= texSize;
            texPos12 /= texSize;
        
            // Sample with bilinear optimization (9 samples instead of 16)
            vec4 result = vec4(0.0);
            result += sampleLevel0(tex, vec2(texPos0.x, texPos0.y)) * wx[0] * wy[0];
            result += sampleLevel0(tex, vec2(texPos12.x, texPos0.y)) * wx12 * wy[0];
            result += sampleLevel0(tex, vec2(texPos3.x, texPos0.y)) * wx[3] * wy[0];
            result += sampleLevel0(tex, vec2(texPos0.x, texPos12.y)) * wx[0] * wy12;
            result += sampleLevel0(tex, vec2(texPos12.x, texPos12.y)) * wx12 * wy12;
            result += sampleLevel0(tex, vec2(texPos3.x, texPos12.y)) * wx[3] * wy12;
            result += sampleLevel0(tex, vec2(texPos0.x, texPos3.y)) * wx[0] * wy[3];
            result += sampleLevel0(tex, vec2(texPos12.x, texPos3.y)) * wx12 * wy[3];
            result += sampleLevel0(tex, vec2(texPos3.x, texPos3.y)) * wx[3] * wy[3];
        
            return result;
        }
        
        // Smart texture sampling that only interpolates when scaling
        vec4 sampleTextureAdaptive(sampler2D tex, vec2 uv) {
        #ifdef GL_ES // Don't do extra interpolation on embedded platforms, it's too slow
            return texture(tex, uv);
        #endif
            vec2 texSize = vec2(textureSize(tex, 0));
        
            vec2 dudx = dFdx(uv) * texSize;
            vec2 dudy = dFdy(uv) * texSize;
        
            float footprint = max(length(dudx), length(dudy));
        
            // Upscale using Mitchell-Netravali
            if (footprint < 0.995)
                return textureMitchellNetravali(tex, uv);
        
            return texture(tex, uv, -0.5);
        }
        
        float sdfCov(float D, float sdfscale)
        {
          return clamp((D - 0.5f)/sdfscale + radius, 0.0f, 1.0f);
        }
        
        float superSDF(sampler2D tex, vec2 st)
        {
          vec2 tex_wh = vec2(textureSize(tex, 0));
          vec2 dst_dx = dFdx(st) * tex_wh;
          float s = (32.0f/255.0f) * length(dst_dx);
          s = 0.5f*s;  // subpixel adjustment
        
          // Subpixel offsets
          float dx = dFdx(st).x / 4.0f;
          float dy = dFdy(st).y / 4.0f;
        
          float d11 = texture(tex, st + vec2(dx, dy)).r;
          float d10 = texture(tex, st + vec2(dx,-dy)).r;
          float d01 = texture(tex, st + vec2(-dx, dy)).r;
          float d00 = texture(tex, st + vec2(-dx,-dy)).r;
        
          return 0.25f*(sdfCov(d11, s) + sdfCov(d10, s) + sdfCov(d01, s) + sdfCov(d00, s));
        }
        
        )" << R"(
        void main(void) {
            vec4 result;
            float scissor = scissorMask(fpos, scissorRadius);
            if(scissor == 0.0f) {
                outColor = vec4(0, 0, 0, 0);
            }
            switch(type) {
            case NSVG_SHADER_FAST_ROUNDEDRECT: {
                vec2 pt = (transformInverse(paintMat) * vec3(fpos,1.0f)).xy;
    
                float oD = sdroundrect(pt, extent, radius) - 0.04f;
                float outerD = fwidth(oD) * 0.5f;
                float iD = oD + 1.0f;
                float innerD = fwidth(iD) * 0.5f;
                float outerRoundedRectAlpha = clamp(inverseLerp(outerD, -outerD, oD), 0.0f, 1.0f);
                float innerRoundedRectAlpha = clamp(inverseLerp(innerD, -innerD, iD), 0.0f, 1.0f);
                result = vec4(mix(convertColour(outerCol).rgba, convertColour(innerCol).rgba, innerRoundedRectAlpha).rgba * outerRoundedRectAlpha) * scissor;
                outColor = result;
                return;
            }
            case NSVG_SHADER_OBJECT_RECT: {
                vec2 pt = (transformInverse(paintMat) * vec3(fpos,1.0f)).xy;
    
                int flagType = (stateData >> 9) & 0x03;     // 2 bits
                float flagSize = 5.0f;
                bool objectOutline = bool((stateData >> 11) & 0x01); // 1 bit (off or on)
                float offset = objectOutline ? 0.2f : -0.5f;
                float flag;
    
                switch (flagType){
                    case 1: { // triangle flag top bottom
                        vec2 flagPosTopBottom = vec2(pt.x, -abs(pt.y)) - vec2(extent.x + offset, -extent.y);
                        vec2 rPoint = rotatePoint(flagPosTopBottom, 0.7854f); // 45 in radians
                        flag = sdroundrect(rPoint, vec2(flagSize), 0.0f);
                        break;
                    }
                    case 2: { // triangle flag top only
                        vec2 flagPosTop = pt - vec2(extent.x + offset, -extent.y);
                        vec2 rPoint2 = rotatePoint(flagPosTop, 0.7854f); // 45 in radians
                        flag = sdroundrect(rPoint2, vec2(flagSize), 0.0f);
                        break;
                    }
                    case 3: { // composite square & triangle top / bottom
                        flagSize = 3.5f;
                        float hypot = length(vec2(flagSize));
                        vec2 messageFlag = vec2(pt.x, -abs(pt.y)) - vec2(extent.x + offset, -extent.y + hypot);
                        vec2 rPoint3 = rotatePoint(messageFlag, 0.7854f); // 45 in radians
                        float triangle = sdroundrect(rPoint3, vec2(flagSize), 0.0f);
                        float squareMid = (extent.y - flagSize) * 0.5f;
                        float square = sdroundrect(vec2(messageFlag.x, messageFlag.y - squareMid), vec2(hypot, squareMid), 0.0f);
                        flag = min(triangle, square); // union
                        break;
                    }
                    default:
                        break;
                    }
    
                float oD = sdroundrect(pt, extent, radius) - 0.04f; // Calculate outer rectangle
    
                if (objectOutline) {
                    oD = max(oD, -flag); // subtract flag shape from background
                }
    
                float flagD = fwidth(flag) * 0.5f;
                float triFlagShape = clamp(inverseLerp(flagD, -flagD, flag), 0.0f, 1.0f);
    
                float outerD = fwidth(oD) * 0.5f;
                // Use same SDF and reduce by 1px for border
                float iD = oD + 1.0f;
                float innerD = fwidth(iD) * 0.5f;
    
                float outerRoundedRectAlpha = clamp(inverseLerp(outerD, -outerD, oD), 0.0f, 1.0f);
                float innerRoundedRectAlpha = clamp(inverseLerp(innerD, -innerD, iD), 0.0f, 1.0f);
    
                vec4 finalColor;
                if (objectOutline) {
                    finalColor = mix(convertColour(outerCol), convertColour(innerCol), innerRoundedRectAlpha);
                } else {
                    finalColor = mix(convertColour(outerCol), mix(convertColour(innerCol), convertColour(dashCol), triFlagShape), innerRoundedRectAlpha);
                }
    
                outColor = vec4(finalColor * outerRoundedRectAlpha) * scissor;
                return;
            }
            case NSVG_SHADER_FILLCOLOR: { // fill color
                float strokeAlpha = strokeMask(getLineStyle());
                outColor = convertColour(innerCol) * strokeAlpha * scissor;
                return;
            }
            case NSVG_DOUBLE_STROKE:
            case NSVG_DOUBLE_STROKE_GRAD:
            case NSVG_DOUBLE_STROKE_ACTIVITY:
            case NSVG_DOUBLE_STROKE_GRAD_ACTIVITY: {
                // Deal with path flipping here - instead of in geometry
                // We only need to flip the Y as the X (width) of the line is symmetrical currently
                float revUVy = (getReverse()) ? 0.5f - uv.y : uv.y;
                vec2 uvLine = vec2(uv.x, revUVy * lineLength);
    
                float zoom = 1.0f / length(transformInverse(paintMat)[0].xy);
                float lineScale = mix(1.0, 2.0, smoothstep(1.0, 0.5, zoom));
                float innerSize = 0.22 * lineScale;
                float outerSize = 0.49 * lineScale;
    
                float innerMask = min(1.0f, (1.0f-abs(ftcoord.x*2.0f-1.0f))*strokeMult);
    
                float seg = sdSegment(uvLine, vec2(0.0f), vec2(0.0f, lineLength * 0.5f));
                float outerSeg = seg - outerSize;
                float outerDelta = fwidth(outerSeg);
                float outerShape = clamp(inverseLerp(outerDelta, -outerDelta, outerSeg), 0.0f, 1.0f);
                float innerSeg = seg - innerSize;
                float innerDelta = fwidth(innerSeg);
                float innerShape = clamp(inverseLerp(innerDelta, -innerDelta, innerSeg), 0.0f, 1.0f);
    
                float pattern = 0.0f;
                if (radius > 0.0f) {
                    pattern = dashed(uvLine, radius, innerSize, feather);
                }
                float activity = 0.0f;
                if (type == NSVG_DOUBLE_STROKE_ACTIVITY || type == NSVG_DOUBLE_STROKE_GRAD_ACTIVITY) {
                    activity = dashed(vec2(uvLine.x, uvLine.y - (offset * 3.0f)), 3.0f, 0.4f * lineScale, feather);
                }
                if (type == NSVG_DOUBLE_STROKE) {
                    result = mix(mix(convertColour(outerCol), convertColour(innerCol), smoothstep(0.0, 1.0, innerShape)), convertColour(dashCol), pattern * innerShape) * outerShape * scissor * innerMask;
                } else if (type == NSVG_DOUBLE_STROKE_ACTIVITY) {
                    vec4 overlay = mix(convertColour(outerCol), vec4(convertColour(innerCol).rgb * 0.8f, 1.0f), activity);
                    vec4 mixedResult = mix(overlay, convertColour(innerCol), innerShape);
                    result = mixedResult * outerShape * scissor * innerMask;
                } else if (type == NSVG_DOUBLE_STROKE_GRAD || type == NSVG_DOUBLE_STROKE_GRAD_ACTIVITY) {
                    vec4 cable;
                    if (type == NSVG_DOUBLE_STROKE_GRAD) {
                        cable = mix(mix(convertColour(outerCol), convertColour(innerCol), smoothstep(0.0, 1.0, innerShape)), convertColour(dashCol), pattern * innerShape);
                    } else {
                        vec4 overlay = mix(convertColour(outerCol), vec4(convertColour(innerCol).rgb * 0.8f, 1.0f), activity);
                        vec4 mixedResult = mix(overlay, convertColour(innerCol), innerShape);
                        cable = mixedResult * outerShape;
                    }
                    float scaledUV = uv.y * 2.0f * lineLength;
                    // Define the proportion of the line length where the fade should occur
                    float fadeProportion = 0.3;
    
                    // Calculate the fade range based on the line length, and make connections shorter than 60px solid
                    float fadeRange = max(fadeProportion * lineLength, 60.0f);
    
                    float fade = smoothstep(0.4, fadeRange, scaledUV) * smoothstep(0.4, fadeRange, lineLength - scaledUV);
    
                    // limit fade transparency so it doesn't become fully transparent
                    fade = min(fade, 0.7f);
    
                    result = (mix(cable, vec4(0.0), fade)) * outerShape * scissor * innerMask;
                }
                outColor = result;
                return;
            }
            case NSVG_SMOOTH_GLOW: {
                vec2 pt = (transformInverse(paintMat) * vec3(fpos, 1.0)).xy;
    
                float blurRadius = clamp(radius, 2.0f, 20.0f) + feather;
                float distShadow = clamp(sigmoid(sdroundrect(pt, extent - vec2(blurRadius), blurRadius) / feather), 0.0f, 1.0f);
                float distRect = clamp(sdroundrect(pt, extent - vec2(5.5f), radius), 0.0f, 1.0f);
                vec4 col = vec4(convertColour(innerCol) * (1.0f - distShadow));
                col = mix(vec4(0.0f), col, distRect);
                outColor = normalBlend(vec4(0.0f), col) * scissor;
                return;
            }
            case NSVG_SHADER_FILLGRAD: {
                // Calculate gradient color using box gradient
                vec2 pt = (transformInverse(paintMat) * vec3(fpos,1.0f)).xy;
    
                int lineStyle = getLineStyle();
                float strokeAlpha = strokeMask(lineStyle);
                float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5f) / feather, 0.0f, 1.0f);
                vec4 color = mix(convertColour(innerCol), convertColour(outerCol), d);
                // Combine alpha
                color *= strokeAlpha * scissor;
                outColor = color;
                return;
            }
            case NSVG_SHADER_FILLIMG: {
                // Calculate color from texture
                vec2 pt = (transformInverse(paintMat) * vec3(fpos,1.0f)).xy / extent;
                float strokeAlpha = strokeMask(getLineStyle());
                vec4 color = sampleTextureAdaptive(tex, vec2(pt.x, getReverse() ? 1.0f - pt.y : pt.y));
    
                int texType = getTexType();
                if (texType == NSVG_TEXTURE_ALPHA) color = vec4(color.x);
                else if (texType == NSVG_TEXTURE_ARGB) color = color;
                else if (texType == NSVG_TEXTURE_ARGB_SRGB) color = linearToSrgb(color);
                outColor = color.rgba * strokeAlpha * scissor;
                outColor.a *= getRawColour(innerCol).a;
                return;
            }
            case NSVG_SHADER_FILLIMG_ALPHA: {
                // Calculate alpha from texture
                vec2 pt = (transformInverse(paintMat) * vec3(fpos,1.0f)).xy / extent;
                float strokeAlpha = strokeMask(getLineStyle());
                vec4 color = texture(tex, pt);
                float alpha = color.a;
                if (getTexType() == NSVG_TEXTURE_ALPHA) alpha = color.r;
                vec3 maskColor = getRawColour(innerCol).rgb;
                outColor = vec4(maskColor * alpha, alpha) * strokeAlpha * scissor;
                return;
            }
            case NSVG_SHADER_TEXT: {
                float cov = scissor * superSDF(tex, ftcoord);
                outColor = vec4(cov) * convertColour(innerCol);
                return;
            }
            default:
                return;
            }
        }
    )";
    
    glnvg__checkError(gl, "init");
    
    if (glnvg__createShader(&gl->shader, shaderHeader.str().c_str(), fillVertShader, fillFragShader.str().c_str()) == 0)
        return 0;
    
    glnvg__checkError(gl, "uniform locations");
    glnvg__getUniforms(&gl->shader);
    
    // Create dynamic vertex array
    
    glGenVertexArrays(1, &gl->vertArr);
    glGenBuffers(1, &gl->vertBuf);
    
    // Create UBOs
    glUniformBlockBinding(gl->shader.prog, gl->shader.loc[GLNVG_LOC_FRAG], GLNVG_FRAG_BINDING);
    glGenBuffers(1, &gl->fragBuf);
    glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &align);
    
    gl->fragSize = sizeof(GLNVGfragUniforms) + align - sizeof(GLNVGfragUniforms) % align;
    
    // Some platforms does not allow to have samples to unset textures.
    // Create empty one which is bound when there's no texture specified.
    gl->dummyTex = nvg__renderCreateTexture(gl, NVG_TEXTURE_ALPHA, 1, 1, 0, NULL);
    
    glnvg__checkError(gl, "create done");
    
    glFinish();
    
    return 1;
}

static int glnvg__convertPaint(GLNVGcontext* gl, GLNVGfragUniforms* frag, NVGpaint* paint,
                               NVGscissor* scissor, float width, float fringe, float lineLength, int lineStyle, bool lineReversed = false)
{
    memset(frag, 0, sizeof(*frag));
    
    frag->type = paint->type;
    frag->innerCol = paint->innerColor.rgba32;
    frag->outerCol = paint->outerColor.rgba32;
    frag->dashCol = paint->dashColor.rgba32;
    frag->stateData |= glnvg__packStateDataUniform(PACK_LINE_STYLE, lineStyle);
    frag->radius = paint->radius;
    frag->feather = paint->feather;
    memcpy(frag->extent, paint->extent, sizeof(frag->extent));
    frag->strokeMult = (width * 0.5f + fringe * 0.5f) / fringe;
    frag->lineLength = lineLength;
    memcpy(frag->paintMat, paint->xform, 6 * sizeof(float));
    
    if (scissor->extent[0] < -0.5f || scissor->extent[1] < -0.5f) {
        memset(frag->scissorMat, 0, sizeof(frag->scissorMat));
        frag->scissorExt[0] = 1.0f;
        frag->scissorExt[1] = 1.0f;
        frag->scissorRadius = 0.0f;
    } else {
        memcpy(frag->scissorMat, scissor->xform, 6 * sizeof(float));
        frag->scissorExt[0] = scissor->extent[0];
        frag->scissorExt[1] = scissor->extent[1];
        frag->scissorRadius = scissor->radius;
    }
    
    switch (paint->type) {
        case PAINT_TYPE_FILLIMG_ALPHA:
        case PAINT_TYPE_FILLIMG: {
            GLNVGtexture* tex = glnvg__findTexture(gl, paint->image);
            if (tex == NULL) return 0;
            frag->stateData |= glnvg__packStateDataUniform(PACK_TEX_TYPE, tex->type);
            if ((tex->flags & NVG_IMAGE_FLIPY) != 0) {
                frag->stateData |= glnvg__packStateDataUniform(PACK_REVERSE, true);
            }
            break;
        }
        case PAINT_TYPE_OBJECT_RECT: {
            frag->stateData |= glnvg__packStateDataUniform(PACK_FLAG_TYPE, paint->flag_type);
            frag->stateData |= glnvg__packStateDataUniform(PACK_OBJECT_STYLE, paint->flag_outline);
            break;
        }
        case PAINT_TYPE_DOUBLE_STROKE_GRAD_ACTIVITY:
        case PAINT_TYPE_DOUBLE_STROKE_ACTIVITY:
        case PAINT_TYPE_DOUBLE_STROKE_GRAD:
        case PAINT_TYPE_DOUBLE_STROKE:
            if(paint->type == PAINT_TYPE_DOUBLE_STROKE_GRAD_ACTIVITY ||
               paint->type == PAINT_TYPE_DOUBLE_STROKE_ACTIVITY)
            {
                frag->offset = paint->offset;
                frag->strokeMult = (width * 0.18f + fringe * 0.5f) / fringe;
            }
            else {
                frag->strokeMult = (width * 0.11f + fringe * 0.5f) / fringe;
                frag->stateData |= glnvg__packStateDataUniform(PACK_REVERSE, lineReversed);
            }
            if(width * 0.4f < fringe)
            {
                float alphaMult = std::clamp((width * 0.4f) / fringe, 0.0f, 1.0f);
                alphaMult *= alphaMult;
                frag->innerCol = (frag->innerCol & 0xFFFFFF00) | (uint32_t)((frag->innerCol & 0xFF) * alphaMult);
            }
            
            
            
            break;
        default: break;
    }
    return 1;
}

static GLNVGfragUniforms* nvg__fragUniformPtr(GLNVGcontext* gl, int i);

static void glnvg__setUniforms(GLNVGcontext* gl, int uniformOffset, int image)
{
    // Bind the uniform buffer only if it's not already bound
    static int lastUniformOffset = -1;
    if (uniformOffset != lastUniformOffset) {
        glBindBufferRange(GL_UNIFORM_BUFFER, GLNVG_FRAG_BINDING, gl->fragBuf, uniformOffset, sizeof(GLNVGfragUniforms));
        lastUniformOffset = uniformOffset;
    }
    
    // Determine which texture to use
    GLNVGtexture* tex = (image != 0) ? glnvg__findTexture(gl, image) : glnvg__findTexture(gl, gl->dummyTex);
    
    // Bind the texture only if it's not already bound
    static GLuint lastTexture = 0;
    GLuint newTexture = (tex != NULL) ? tex->tex : 0;
    if (newTexture != lastTexture) {
        glBindTexture(GL_TEXTURE_2D, newTexture);
        lastTexture = newTexture;
    }
    
    glnvg__checkError(gl, "tex paint tex");
}

void nvg__renderViewport(void* uptr, float width, float height, float devicePixelRatio)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    gl->view[0] = width;
    gl->view[1] = height;
    gl->devicePixelRatio = devicePixelRatio;
}

static void glnvg__fill(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
    int i, npaths = call->pathCount;
    
    // Draw shapes
    glEnable(GL_STENCIL_TEST);
    glnvg__stencilMask(gl, 0xff);
    glnvg__stencilFunc(gl, GL_ALWAYS, 0, 0xff);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    
    // set bindpoint for solid loc
    glnvg__setUniforms(gl, call->uniformOffset, 0);
    glnvg__checkError(gl, "fill simple");
    
    glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
    glStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR_WRAP);
    glDisable(GL_CULL_FACE);
    for (i = 0; i < npaths; i++)
        glDrawArrays(GL_TRIANGLE_FAN, paths[i].fillOffset, paths[i].fillCount);
    glEnable(GL_CULL_FACE);
    
    // Draw anti-aliased pixels
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    
    glnvg__setUniforms(gl, call->uniformOffset + gl->fragSize, call->image);
    glnvg__checkError(gl, "fill fill");
    
    // Anti-aliasing
    glnvg__stencilFunc(gl, GL_EQUAL, 0x00, 0xff);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    // Draw fringes
    for (i = 0; i < npaths; i++)
        glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
    
    // Draw fill
    glnvg__stencilFunc(gl, GL_NOTEQUAL, 0x0, 0xff);
    glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
    glDrawArrays(GL_TRIANGLE_STRIP, call->triangleOffset, call->triangleCount);
    
    glDisable(GL_STENCIL_TEST);
}

static void glnvg__convexFill(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
    int i, npaths = call->pathCount;
    
    glnvg__setUniforms(gl, call->uniformOffset, call->image);
    glnvg__checkError(gl, "convex fill");
    
    for (i = 0; i < npaths; i++) {
        glDrawArrays(GL_TRIANGLE_FAN, paths[i].fillOffset, paths[i].fillCount);
        // Draw fringes
        if (paths[i].strokeCount > 0) {
            glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
        }
    }
}

static void glnvg__stroke(GLNVGcontext* gl, GLNVGcall* call)
{
    GLNVGpath* paths = &gl->paths[call->pathOffset];
    int npaths = call->pathCount, i;
    
    glnvg__setUniforms(gl, call->uniformOffset, call->image);
    glnvg__checkError(gl, "stroke fill");
    // Draw Strokes
    for (i = 0; i < npaths; i++)
        glDrawArrays(GL_TRIANGLE_STRIP, paths[i].strokeOffset, paths[i].strokeCount);
}

static void glnvg__triangles(GLNVGcontext* gl, GLNVGcall* call)
{
    glnvg__setUniforms(gl, call->uniformOffset, call->image);
    glnvg__checkError(gl, "triangles fill");
    
    glDrawArrays(GL_TRIANGLES, call->triangleOffset, call->triangleCount);
}

void nvg__renderCancel(void* uptr) {
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    gl->nverts = 0;
    gl->npaths = 0;
    gl->ncalls = 0;
    gl->nuniforms = 0;
}

static GLNVGblend glnvg__blendCompositeOperation(NVGcompositeOperationState op)
{
    GLNVGblend blend;
    if (op.srcRGB == 0 || op.dstRGB == 0 || op.srcAlpha == 0 || op.dstAlpha == 0)
    {
        blend.srcRGB = GL_ONE;
        blend.dstRGB = GL_ONE_MINUS_SRC_ALPHA;
        blend.srcAlpha = GL_ONE;
        blend.dstAlpha = GL_ONE_MINUS_SRC_ALPHA;
    }
    else {
        blend.srcRGB = GLNVGBlendFactors[op.srcRGB];
        blend.dstRGB = GLNVGBlendFactors[op.dstRGB];
        blend.srcAlpha = GLNVGBlendFactors[op.srcAlpha];
        blend.dstAlpha = GLNVGBlendFactors[op.dstAlpha];
    }
    return blend;
}

void nvg__renderFlush(void* uptr, NVGscissorBounds scissor)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    int i;
    
    if (gl->ncalls > 0) {
        
        // Setup require GL state.
        glUseProgram(gl->shader.prog);

        glDisable(GL_SCISSOR_TEST);
        glClear(GL_STENCIL_BUFFER_BIT);
        glEnable(GL_SCISSOR_TEST);

        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        glFrontFace(GL_CCW);
        glEnable(GL_BLEND);
        glDisable(GL_DEPTH_TEST);
        glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
        glStencilMask(0xffffffff);
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
        glStencilFunc(GL_ALWAYS, 0, 0xffffffff);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, 0);
        
        glScissor(scissor.x, (gl->view[1] * gl->devicePixelRatio) - (scissor.y + scissor.h), scissor.w, scissor.h);
        
        gl->boundTexture = 0;
        gl->stencilMask = 0xffffffff;
        gl->stencilFunc = GL_ALWAYS;
        gl->stencilFuncRef = 0;
        gl->stencilFuncMask = 0xffffffff;
        gl->blendFunc.srcRGB = GL_INVALID_ENUM;
        gl->blendFunc.srcAlpha = GL_INVALID_ENUM;
        gl->blendFunc.dstRGB = GL_INVALID_ENUM;
        gl->blendFunc.dstAlpha = GL_INVALID_ENUM;
        
        // Upload UBO
        // Only reallocate if buffer size is larger than current size
        // Else substitute the buffer into the current buffer
        glBindBuffer(GL_UNIFORM_BUFFER, gl->fragBuf);
        int uniform_size = gl->nuniforms * gl->fragSize;
        if (uniform_size > gl->current_uniform_size) {
            glBufferData(GL_UNIFORM_BUFFER, uniform_size, gl->uniforms, GL_DYNAMIC_DRAW);
            gl->current_uniform_size = uniform_size;
        }
        else
            glBufferSubData(GL_UNIFORM_BUFFER, 0, uniform_size, gl->uniforms);
        
        // Upload vertex data
        // Only reallocate if buffer size is larger than current size
        // Else substitute the buffer into the current buffer
        glBindVertexArray(gl->vertArr);
        glBindBuffer(GL_ARRAY_BUFFER, gl->vertBuf);
        int array_size = gl->nverts * sizeof(NVGvertex);
        if (array_size > gl->current_vert_array_size){
            glBufferData(GL_ARRAY_BUFFER, array_size, gl->verts, GL_DYNAMIC_DRAW);
            gl->current_vert_array_size = array_size;
        }
        else
            glBufferSubData(GL_ARRAY_BUFFER, 0, array_size, gl->verts);
        
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(NVGvertex), (const GLvoid*)(size_t)0);
        glVertexAttribPointer(1, 4, GL_SHORT, GL_TRUE, sizeof(NVGvertex), (const GLvoid*)(0 + 2*sizeof(float)));
        
        // Set view and texture just once per frame.
        glUniform1i(gl->shader.loc[GLNVG_LOC_TEX], 0);
        glUniform2fv(gl->shader.loc[GLNVG_LOC_VIEWSIZE], 1, gl->view);
        
        glBindBuffer(GL_UNIFORM_BUFFER, gl->fragBuf);
        
        for (i = 0; i < gl->ncalls; i++) {
            GLNVGcall* call = &gl->calls[i];
            glnvg__blendFuncSeparate(gl,&call->blendFunc);
            switch (call->type) {
                case GLNVG_FILL:
                    glnvg__fill(gl, call);
                    break;
                case GLNVG_CONVEXFILL:
                    glnvg__convexFill(gl, call);
                    break;
                case GLNVG_STROKE:
                    glnvg__stroke(gl, call);
                    break;
                case GLNVG_TRIANGLES:
                    glnvg__triangles(gl, call);
                    break;
                default:
                    break;
            }
        }
        
        glDisableVertexAttribArray(0);
        glDisableVertexAttribArray(1);
        glBindVertexArray(0);
        
        glDisable(GL_CULL_FACE);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glUseProgram(0);
        glnvg__bindTexture(gl, 0);
    }
    
    // Reset calls
    gl->nverts = 0;
    gl->npaths = 0;
    gl->ncalls = 0;
    gl->nuniforms = 0;
}

static int glnvg__maxVertCount(const NVGpath* paths, int npaths)
{
    int i, count = 0;
    for (i = 0; i < npaths; i++) {
        count += paths[i].nfill;
        count += paths[i].nstroke;
    }
    return count;
}

static GLNVGcall* glnvg__allocCall(GLNVGcontext* gl)
{
    GLNVGcall* ret = NULL;
    if (gl->ncalls+1 > gl->ccalls) {
        GLNVGcall* calls;
        int ccalls = glnvg__maxi(gl->ncalls+1, 128) + gl->ccalls/2; // 1.5x Overallocate
        calls = (GLNVGcall*)realloc(gl->calls, sizeof(GLNVGcall) * ccalls);
        if (calls == NULL) return NULL;
        gl->calls = calls;
        gl->ccalls = ccalls;
    }
    ret = &gl->calls[gl->ncalls++];
    memset(ret, 0, sizeof(GLNVGcall));
    return ret;
}

static int glnvg__allocPaths(GLNVGcontext* gl, int n)
{
    int ret = 0;
    if (gl->npaths+n > gl->cpaths) {
        GLNVGpath* paths;
        int cpaths = glnvg__maxi(gl->npaths + n, 128) + gl->cpaths/2; // 1.5x Overallocate
        paths = (GLNVGpath*)realloc(gl->paths, sizeof(GLNVGpath) * cpaths);
        if (paths == NULL) return -1;
        gl->paths = paths;
        gl->cpaths = cpaths;
    }
    ret = gl->npaths;
    gl->npaths += n;
    return ret;
}

static int glnvg__allocVerts(GLNVGcontext* gl, int n)
{
    int ret = 0;
    if (gl->nverts+n > gl->cverts) {
        NVGvertex* verts;
        int cverts = glnvg__maxi(gl->nverts + n, 4096) + gl->cverts/2; // 1.5x Overallocate
        verts = (NVGvertex*)realloc(gl->verts, sizeof(NVGvertex) * cverts);
        if (verts == NULL) return -1;
        gl->verts = verts;
        gl->cverts = cverts;
    }
    ret = gl->nverts;
    gl->nverts += n;
    return ret;
}

static int glnvg__allocFragUniforms(GLNVGcontext* gl, int n)
{
    int ret = 0, structSize = gl->fragSize;
    if (gl->nuniforms+n > gl->cuniforms) {
        unsigned char* uniforms;
        int cuniforms = glnvg__maxi(gl->nuniforms+n, 128) + gl->cuniforms/2; // 1.5x Overallocate
        uniforms = (unsigned char*)realloc(gl->uniforms, structSize * cuniforms);
        if (uniforms == NULL) return -1;
        gl->uniforms = uniforms;
        gl->cuniforms = cuniforms;
    }
    ret = gl->nuniforms * structSize;
    gl->nuniforms += n;
    return ret;
}

static GLNVGfragUniforms* nvg__fragUniformPtr(GLNVGcontext* gl, int i)
{
    return (GLNVGfragUniforms*)&gl->uniforms[i];
}

static void glnvg__vset(NVGvertex* vtx, float x, float y, float u, float v)
{
    int16_t scaling_factor = 1 << 14;
    vtx->x = x;
    vtx->y = y;
    vtx->u = u * scaling_factor;
    vtx->v = v * scaling_factor;
}

void nvg__renderFill(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                     const float* bounds, const NVGpath* paths, int npaths)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    NVGvertex* quad;
    GLNVGfragUniforms* frag;
    int i, maxverts, offset;
    
    if (call == NULL) return;
    
    call->type = GLNVG_FILL;
    call->triangleCount = 4;
    call->pathOffset = glnvg__allocPaths(gl, npaths);
    if (call->pathOffset == -1) goto error;
    call->pathCount = npaths;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);
    
    if (npaths == 1 && paths[0].convex)
    {
        call->type = GLNVG_CONVEXFILL;
        call->triangleCount = 0;    // Bounding box fill quad not needed for convex fill
    }
    
    // Allocate vertices for all the paths.
    maxverts = glnvg__maxVertCount(paths, npaths) + call->triangleCount;
    offset = glnvg__allocVerts(gl, maxverts);
    if (offset == -1) goto error;
    
    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &gl->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nfill > 0) {
            copy->fillOffset = offset;
            copy->fillCount = path->nfill;
            memcpy(&gl->verts[offset], path->fill, sizeof(NVGvertex) * path->nfill);
            offset += path->nfill;
        }
        if (path->nstroke > 0) {
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&gl->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }
    
    // Setup uniforms for draw calls
    if (call->type == GLNVG_FILL) {
        // Quad
        call->triangleOffset = offset;
        quad = &gl->verts[call->triangleOffset];
        glnvg__vset(&quad[0], bounds[2], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[1], bounds[2], bounds[1], 0.5f, 1.0f);
        glnvg__vset(&quad[2], bounds[0], bounds[3], 0.5f, 1.0f);
        glnvg__vset(&quad[3], bounds[0], bounds[1], 0.5f, 1.0f);
        
        call->uniformOffset = glnvg__allocFragUniforms(gl, 2);
        if (call->uniformOffset == -1) goto error;
        // Simple shader for stencil
        frag = nvg__fragUniformPtr(gl, call->uniformOffset);
        memset(frag, 0, sizeof(*frag));
        // Fill shader
        glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->uniformOffset + gl->fragSize), paint, scissor, fringe, fringe, -1.0f, 0.0f, 0);
    } else {
        call->uniformOffset = glnvg__allocFragUniforms(gl, 1);
        if (call->uniformOffset == -1) goto error;
        // Fill shader
        frag = nvg__fragUniformPtr(gl, call->uniformOffset);
        glnvg__convertPaint(gl, frag, paint, scissor, fringe, fringe, -1.0f, 0.0f, 0);
    }
    
    return;
    
error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

void nvg__renderStroke(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
                       float strokeWidth, int lineStyle, float lineLength, const NVGpath* paths, int npaths)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    int i, maxverts, offset;
    
    bool lineReversed = false;
    
    if (call == NULL) return;
    
    call->type = GLNVG_STROKE;
    call->pathOffset = glnvg__allocPaths(gl, npaths);
    if (call->pathOffset == -1) goto error;
    call->pathCount = npaths;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);
    
    // Allocate vertices for all the paths.
    maxverts = glnvg__maxVertCount(paths, npaths);
    offset = glnvg__allocVerts(gl, maxverts);
    if (offset == -1) goto error;
    
    for (i = 0; i < npaths; i++) {
        GLNVGpath* copy = &gl->paths[call->pathOffset + i];
        const NVGpath* path = &paths[i];
        memset(copy, 0, sizeof(GLNVGpath));
        if (path->nstroke) {
            lineReversed = path->reversed;
            copy->strokeOffset = offset;
            copy->strokeCount = path->nstroke;
            memcpy(&gl->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
            offset += path->nstroke;
        }
    }
    
    // Fill shader
    call->uniformOffset = glnvg__allocFragUniforms(gl, 1);
    if (call->uniformOffset == -1) goto error;
    glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->uniformOffset), paint, scissor, strokeWidth, fringe, lineLength, lineStyle, lineReversed);
    
    return;
    
error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

void nvg__renderTriangles(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor,
                          const NVGvertex* verts, int nverts, float fringe, int text)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGcall* call = glnvg__allocCall(gl);
    GLNVGfragUniforms* frag;
    
    if (call == NULL) return;
    
    call->type = GLNVG_TRIANGLES;
    call->image = paint->image;
    call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);
    
    // Allocate vertices for all the paths.
    call->triangleOffset = glnvg__allocVerts(gl, nverts);
    if (call->triangleOffset == -1) goto error;
    call->triangleCount = nverts;
    
    memcpy(&gl->verts[call->triangleOffset], verts, sizeof(NVGvertex) * nverts);
    
    // Fill shader
    call->uniformOffset = glnvg__allocFragUniforms(gl, 1);
    if (call->uniformOffset == -1) goto error;
    frag = nvg__fragUniformPtr(gl, call->uniformOffset);
    glnvg__convertPaint(gl, frag, paint, scissor, 1.0f, fringe, -1.0f, 0.0f, 0);
    if(text) {
        frag->type = PAINT_TYPE_TEXT;
    }
    
    return;
    
error:
    // We get here if call alloc was ok, but something else is not.
    // Roll back the last call to prevent drawing it.
    if (gl->ncalls > 0) gl->ncalls--;
}

void nvg__renderDelete(void* uptr)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    int i;
    if (gl == NULL) return;
    
    glnvg__deleteShader(&gl->shader);
    
    if (gl->fragBuf != 0)
        glDeleteBuffers(1, &gl->fragBuf);
    
    if (gl->vertArr != 0)
        glDeleteVertexArrays(1, &gl->vertArr);
    
    if (gl->vertBuf != 0)
        glDeleteBuffers(1, &gl->vertBuf);
    
    for (i = 0; i < gl->ntextures; i++) {
        if (gl->textures[i].tex != 0 && (gl->textures[i].flags & NVG_IMAGE_NODELETE) == 0)
            glDeleteTextures(1, &gl->textures[i].tex);
    }
    free(gl->textures);
    
    free(gl->paths);
    free(gl->verts);
    free(gl->uniforms);
    free(gl->calls);
    
    free(gl);
}

int nvg__isTexture(void* uptr, int image)
{
    GLNVGcontext* gl = (GLNVGcontext*)uptr;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);
    
    if (!tex)
        return 0;
    
    return glIsTexture(tex->tex);
}

#if defined NANOVG_GL3
NVGcontext* nvgCreateGL3(int flags)
#elif defined NANOVG_GLES3
NVGcontext* nvgCreateGLES3(int flags)
#endif
{
    NVGcontext* ctx = NULL;
    GLNVGcontext* gl = (GLNVGcontext*)malloc(sizeof(GLNVGcontext));
    if (gl == NULL) goto error;
    memset(gl, 0, sizeof(GLNVGcontext));
    
    gl->flags = flags;
    
    // If the context is recreated, force opengl to re-upload the full UBO & VBO
    gl->current_uniform_size = -1;
    gl->current_vert_array_size = -1;
    
    ctx = nvgCreateInternal((void*)gl);
    if (ctx == NULL) goto error;
    
    return ctx;
    
error:
    // 'gl' is freed by nvgDeleteInternal.
    if (ctx != NULL) nvgDeleteInternal(ctx);
    return NULL;
}

#if defined NANOVG_GL3
void nvgDeleteGL3(NVGcontext* ctx)
#elif defined NANOVG_GLES3
void nvgDeleteGLES3(NVGcontext* ctx)
#endif
{
    nvgDeleteInternal(ctx);
}

void nvglClearWithColor(NVGcolor col)
{
    glDisable(GL_SCISSOR_TEST);
    
    glClearColor(col.r / 255.0f, col.g / 255.0f, col.b / 255.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    
    glEnable(GL_SCISSOR_TEST);
}

GLuint nvglImageHandle(NVGcontext* ctx, int image)
{
    GLNVGcontext* gl = *(GLNVGcontext**)ctx;
    GLNVGtexture* tex = glnvg__findTexture(gl, image);
    return tex->tex;
}

#endif /* NANOVG_GL_IMPLEMENTATION */
