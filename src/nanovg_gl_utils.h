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
#ifndef NANOVG_GL_UTILS_H
#define NANOVG_GL_UTILS_H

struct NVGLUframebuffer {
    NVGcontext* ctx;
    GLuint fbo;
    GLuint rbo;
    GLuint texture;
    int image;
};
typedef struct NVGLUframebuffer NVGLUframebuffer;

// Helper function to create GL frame buffer to render to.
void nvgluBindFramebuffer(NVGLUframebuffer* fb);
NVGLUframebuffer* nvgluCreateFramebuffer(NVGcontext* ctx, int w, int h, int imageFlags);
void nvgluGenerateMipmaps(NVGLUframebuffer* fb);
void nvgluDeleteFramebuffer(NVGLUframebuffer* fb);

#ifdef NANOVG_GL_IMPLEMENTATION

static GLint defaultFBO = -1;

// Corner radius (physical pixels) and per-corner enable flags for the blit pass.
static float nvglu__cornerRadius = 0.0f;
static bool  nvglu__roundBL      = false;
static bool  nvglu__roundBR      = false;

void nvgluSetCornerRadius(float radius, bool roundedBottomLeft, bool roundedBottomRight) {
    nvglu__cornerRadius = radius;
    nvglu__roundBL      = roundedBottomLeft;
    nvglu__roundBR      = roundedBottomRight;
}

struct NVGLUblitShader {
    GLuint program;
    GLuint vao;
    GLuint vbo;
    GLint locScreenTexture;
    GLint locRadius;
    GLint locRoundBL;
    GLint locRoundBR;
};

std::unordered_map<NVGcontext*, NVGLUblitShader> blitShaders;

NVGLUblitShader getBlitShaderProgram(NVGcontext* ctx) {
    if(blitShaders.contains(ctx) && glIsProgram(blitShaders[ctx].program)) return blitShaders[ctx];

    const char* vertexShaderSrc =
#if defined NANOVG_GL3
    "#version 150 core\n"
    "#extension GL_ARB_explicit_attrib_location : enable\n"
#elif defined NANOVG_GLES3
    "#version 300 es\n"
#endif
    "in vec2 aPos;\n"
    "in vec2 aTexCoord;\n"
    "out vec2 TexCoord;\n"
    "void main() {\n"
    "    TexCoord = aTexCoord;\n"
    "    gl_Position = vec4(aPos, 0.0, 1.0);\n"
    "}";

    const char* fragmentShaderSrc =
#if defined NANOVG_GL3
    "#version 150 core\n"
#elif defined NANOVG_GLES3
    "#version 300 es\n"
    "precision highp float;\n"
#endif
    "out vec4 FragColor;\n"
    "in vec2 TexCoord;\n"
    "uniform sampler2D screenTexture;\n"
    "uniform float uRadius;\n"
    "uniform int uRoundBL;\n"
    "uniform int uRoundBR;\n"
    "\n"
    "// Coverage for a rounded corner: 1.0 fully inside, 0.0 fully outside,\n"
    "// with a 1px antialiased band on the edge of the quarter circle.\n"
    "float cornerCoverage(vec2 fc, vec2 center, float rad) {\n"
    "    float d = length(fc - center) - rad;\n"
    "    float aa = fwidth(d) * 0.5;\n"
    "    return clamp(0.5 - d / max(aa * 2.0, 0.0001), 0.0, 1.0);\n"
    "}\n"
    "\n"
    "void main() {\n"
    "    if (uRadius > 0.0 && (uRoundBL != 0 || uRoundBR != 0)) {\n"
    "        // gl_FragCoord origin is the bottom-left of the default framebuffer,\n"
    "        // so y < uRadius is the bottom edge.\n"
    "        vec2 res = vec2(textureSize(screenTexture, 0));\n"
    "        vec2 fc  = gl_FragCoord.xy;\n"
    "        float inside = 1.0;\n"
    "\n"
    "        if (uRoundBL != 0 && fc.x < uRadius && fc.y < uRadius) {\n"
    "            inside = cornerCoverage(fc, vec2(uRadius, uRadius), uRadius);\n"
    "        }\n"
    "        else if (uRoundBR != 0 && fc.x > res.x - uRadius && fc.y < uRadius) {\n"
    "            inside = cornerCoverage(fc, vec2(res.x - uRadius, uRadius), uRadius);\n"
    "        }\n"
    "\n"
    "        if (inside <= 0.0) discard;\n"
    "        FragColor = texture(screenTexture, TexCoord) * inside;\n"
    "    } else {\n"
    "        FragColor = texture(screenTexture, TexCoord);\n"
    "    }\n"
    "}";

    GLint success;
    GLchar infoLog[512];

    // Compile Vertex Shader
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSrc, NULL);
    glCompileShader(vertexShader);
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        printf("Vertex Shader Compilation Failed:\n%s\n", infoLog);
    }

    // Compile Fragment Shader
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSrc, NULL);
    glCompileShader(fragmentShader);
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
        printf("Fragment Shader Compilation Failed:\n%s\n", infoLog);
    }

    // Link Shader Program
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);

#if defined NANOVG_GL3
    glBindAttribLocation(shaderProgram, 0, "aPos");
    glBindAttribLocation(shaderProgram, 1, "aTexCoord");
#endif

    glLinkProgram(shaderProgram);
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        printf("Shader Program Linking Failed:\n%s\n", infoLog);
    }

    // Cleanup
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    float quadVertices[] = {
        // Positions    // TexCoords
        -1.0f,  1.0f,  0.0f, 1.0f, // Top-left
        -1.0f, -1.0f,  0.0f, 0.0f, // Bottom-left
        1.0f, -1.0f,  1.0f, 0.0f, // Bottom-right
        1.0f, -1.0f,  1.0f, 0.0f, // Bottom-right
        1.0f,  1.0f,  1.0f, 1.0f, // Top-right
        -1.0f,  1.0f,  0.0f, 1.0f  // Top-left
    };

    GLuint quadVAO, quadVBO;
    glGenVertexArrays(1, &quadVAO);
    glGenBuffers(1, &quadVBO);
    glBindVertexArray(quadVAO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(quadVertices), quadVertices, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    NVGLUblitShader entry;
    entry.program          = shaderProgram;
    entry.vao              = quadVAO;
    entry.vbo              = quadVBO;
    entry.locScreenTexture = glGetUniformLocation(shaderProgram, "screenTexture");
    entry.locRadius        = glGetUniformLocation(shaderProgram, "uRadius");
    entry.locRoundBL       = glGetUniformLocation(shaderProgram, "uRoundBL");
    entry.locRoundBR       = glGetUniformLocation(shaderProgram, "uRoundBR");
    blitShaders[ctx] = entry;
    return entry;
}

// Function to blit framebuffer using a shader
void nvgluBlitFramebuffer(NVGcontext* ctx, NVGLUframebuffer* fb, int x, int y, int w, int h) {
    NVGLUblitShader blit = getBlitShaderProgram(ctx);
    glDisable(GL_SCISSOR_TEST);
    glDisable(GL_CULL_FACE);

    // Bind default framebuffer for rendering
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

    // Clear to fully transparent so corner-discarded pixels show the compositor.
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    bool cornersActive = (nvglu__cornerRadius > 0.0f) && (nvglu__roundBL || nvglu__roundBR);

    // Enable blending so the AA band on the corner edge composites correctly
    // against the cleared (0,0,0,0) destination. Premultiplied source.
    if (cornersActive) {
        glEnable(GL_BLEND);
        glBlendFuncSeparate(GL_ONE, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
    } else {
        glDisable(GL_BLEND);
    }

    // Use the shader program
    glUseProgram(blit.program);

    // Bind the framebuffer texture
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fb->texture);
    glUniform1i(blit.locScreenTexture, 0);
    glUniform1f(blit.locRadius, cornersActive ? nvglu__cornerRadius : 0.0f);
    glUniform1i(blit.locRoundBL, nvglu__roundBL ? 1 : 0);
    glUniform1i(blit.locRoundBR, nvglu__roundBR ? 1 : 0);

    // Draw the fullscreen quad
    glBindVertexArray(blit.vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    // Restore OpenGL states
    glEnable(GL_SCISSOR_TEST);
    glEnable(GL_BLEND);
    glEnable(GL_CULL_FACE);

    // Error check
    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
        printf("OpenGL Error after shader quad blit: %d\n", error);
    }
}

NVGLUframebuffer* nvgluCreateFramebuffer(NVGcontext* ctx, int w, int h, int imageFlags)
{
    GLint defaultFBO;
    GLint defaultRBO;
    NVGLUframebuffer* fb = NULL;

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_RENDERBUFFER_BINDING, &defaultRBO);

    fb = (NVGLUframebuffer*)malloc(sizeof(NVGLUframebuffer));
    if (fb == NULL) goto error;
    memset(fb, 0, sizeof(NVGLUframebuffer));

    fb->image = nvgCreateImageARGB(ctx, w, h, imageFlags | NVG_IMAGE_FLIPY, NULL);
    fb->texture = nvglImageHandle(ctx, fb->image);

    fb->ctx = ctx;

    // frame buffer object
    glGenFramebuffers(1, &fb->fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, fb->fbo);

    // render buffer object
    glGenRenderbuffers(1, &fb->rbo);
    glBindRenderbuffer(GL_RENDERBUFFER, fb->rbo);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_STENCIL_INDEX8, w, h);

    // combine all
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, fb->texture, 0);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, fb->rbo);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
#ifdef GL_DEPTH24_STENCIL8
        // If GL_STENCIL_INDEX8 is not supported, try GL_DEPTH24_STENCIL8 as a fallback.
        // Some graphics cards require a depth buffer along with a stencil.
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, w, h);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, fb->texture, 0);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, fb->rbo);

        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
#endif // GL_DEPTH24_STENCIL8
            goto error;
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glBindRenderbuffer(GL_RENDERBUFFER, defaultRBO);
    return fb;
error:
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glBindRenderbuffer(GL_RENDERBUFFER, defaultRBO);
    nvgluDeleteFramebuffer(fb);
    return NULL;
}

void nvgluBindFramebuffer(NVGLUframebuffer* fb)
{
    if (defaultFBO == -1) glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, fb != NULL ? fb->fbo : defaultFBO);
}

static void nvgluReadPixels(NVGcontext* ctx, NVGLUframebuffer* fb, int x, int y, int width, int height, int total_height, void* data) {
    // Bind the framebuffer associated with the NVGLUframebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, fb->fbo);

    // Set the pixel storage alignment (important for correct data reads)
    glPixelStorei(GL_PACK_ALIGNMENT, 1);

    glReadPixels(x, total_height - y - height, width, height, GL_BGRA, GL_UNSIGNED_BYTE, data);

    auto* buffer = static_cast<uint32_t*>(data);
    for (int row = 0; row < height / 2; row++) {
        std::swap_ranges(buffer + row * width,
                         buffer + (row + 1) * width,
                         buffer + (height - 1 - row) * width);
    }

    // Unbind the framebuffer to restore the default state
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}


void nvgluGenerateMipmaps(NVGLUframebuffer* fb)
{
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fb->texture);
    glGenerateMipmap(GL_TEXTURE_2D);
}

void nvgluDeleteFramebuffer(NVGLUframebuffer* fb)
{
    if (fb == NULL) return;
    if (fb->fbo != 0)
        glDeleteFramebuffers(1, &fb->fbo);
    if (fb->rbo != 0)
        glDeleteRenderbuffers(1, &fb->rbo);
    if (fb->image >= 0)
        nvgDeleteImage(fb->ctx, fb->image);
    fb->ctx = NULL;
    fb->fbo = 0;
    fb->rbo = 0;
    fb->texture = 0;
    fb->image = -1;
    free(fb);
}

#endif // NANOVG_GL_IMPLEMENTATION
#endif // NANOVG_GL_UTILS_H
