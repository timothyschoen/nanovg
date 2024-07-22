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

#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

typedef struct {
  float2 pos [[attribute(0)]];
  half4 tcoord [[attribute(1)]];
} Vertex;

typedef struct {
  float4 pos  [[position]];
  float2 fpos;
  float2 ftcoord;
  float2 uv;
} RasterizerData;

typedef struct  {
  float3x3 scissorMat;
  float3x3 paintMat;
  float4 innerCol;
  float4 outerCol;
  float2 scissorExt;
  float2 scissorScale;
  float2 extent;
  float radius;
  float feather;
  float strokeMult;
  float strokeThr;
  int texType;
  int type;
  float scissorRadius;
  float patternSize;
  float offset;
  int lineStyle;
  float lineLength;
} Uniforms;

float sdroundrect(float2 pt, float2 ext, float rad);
float strokeMask(constant Uniforms& uniforms, float2 ftcoord);

float inverseLerp(float a, float b, float value) {
    return (value - a) / (b - a);
}

float roundedScissorMask(constant Uniforms& uniforms, float2 p, float rad) {
  float2 sc = (abs((uniforms.scissorMat * float3(p,1.0f)).xy));
  float sc2 = sdroundrect(sc, uniforms.scissorExt, rad) - 0.04f;
  float sc3 = fwidth(sc2) * 0.5;
  return clamp(inverseLerp(sc3, -sc3, sc2), 0.0f, 1.0f);
}

float scissorMask(constant Uniforms& uniforms, float2 p) {
  float2 sc = (abs((uniforms.scissorMat * float3(p, 1.0f)).xy)
                  - uniforms.scissorExt) \
              * uniforms.scissorScale;
  sc = saturate(float2(0.5f) - sc);
  return sc.x * sc.y;
}

float sdroundrect(float2 pt, float2 ext, float rad) {
	float2 ext2 = ext - float2(rad, rad);
	float2 d = abs(pt) - ext2;
	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;
}

float strokeMask(constant Uniforms& uniforms, float2 ftcoord) {
  return min(1.0, (1.0 - abs(ftcoord.x * 2.0 - 1.0)) * uniforms.strokeMult) * min(1.0, ftcoord.y);
}

float glow(float2 uv) {
  return smoothstep(0.0, 1.0, 1.0 - 2.0 * abs(uv.x));
}

float capAlpha(float dist) {
  // Replace with your smoothstep function (consider basing it on line thickness)
  return smoothstep(0.4, 0.6, dist);
}

float circleDist(float2 p, float2 center, float d) {
  return distance(center, p) - d;
}

float dashed(float2 uv, float rad, float offset) {
  float2 offset_uv = float2(uv.x, uv.y - offset);
  float fy = fract(offset_uv.y / rad);
  float w = step(fy, rad / 8.0);
  fy *= rad * 0.75;

  if (fy >= rad / 2.666) {
    fy -= rad / 2.666;
  } else if (fy <= rad / 8.0) {
    fy -= rad / 8.0;
  } else {
    fy = 0.0;
  }

  w *= smoothstep(0.0f, 1.0f, (rad * 1.5f) * (0.25f - (offset_uv.x * offset_uv.x  + fy * fy)));
  return w;
}

float dotted(float2 uv) {
  float fy = 4.0 * fract(uv.y / 4.0) - 0.5;
  return smoothstep(0.0, 1.0, 6.0 * (0.25 - dot(uv, uv + float2(0.0, fy))));
}

// Vertex Function
vertex RasterizerData vertexShader(Vertex vert [[stage_in]],
                                   constant float2& viewSize [[buffer(1)]]) {
  RasterizerData out;
  out.ftcoord = (float2)vert.tcoord.xy;
  out.uv = 0.5f * (float2)vert.tcoord.zw;
  out.fpos = vert.pos;
  out.pos = float4(2.0 * vert.pos.x / viewSize.x - 1.0,
                   1.0 - 2.0 * vert.pos.y / viewSize.y,
                   0, 1);
  return out;
}

// Fragment function (No AA)
fragment float4 fragmentShader(RasterizerData in [[stage_in]],
                               constant Uniforms& uniforms [[buffer(0)]],
                               texture2d<float> texture [[texture(0)]],
                               sampler sampler [[sampler(0)]]) {
  float scissor = uniforms.scissorRadius == 0.0f ? scissorMask(uniforms, in.fpos) : roundedScissorMask(uniforms, in.fpos, uniforms.scissorRadius);
  if (scissor == 0)
     discard_fragment();

  float strokeAlpha = 1.0f;
  if(uniforms.lineStyle == 2) strokeAlpha*=dashed(in.uv, uniforms.radius, uniforms.offset);
  if(uniforms.lineStyle == 3) strokeAlpha*=dotted(in.uv);
  if(uniforms.lineStyle == 4) strokeAlpha*=glow(in.uv);

  if (uniforms.type == 4) {  // MNVG_SHADER_FAST_ROUNDEDRECT
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy;
    float oD = sdroundrect(pt, uniforms.extent, uniforms.radius) - 0.04f;
	float outerD = fwidth(oD) * 0.5f;
	float iD = sdroundrect(pt, uniforms.extent - float2(1.0f), uniforms.radius - 1.0f) - 0.04f;
    float innerD = fwidth(iD) * 0.5f;
	float outerRoundedRectAlpha = clamp(inverseLerp(outerD, -outerD, oD), 0.0f, 1.0f);
    float innerRoundedRectAlpha = clamp(inverseLerp(innerD, -innerD, iD), 0.0f, 1.0f);
    float4 result = float4(mix(uniforms.outerCol.rgba, uniforms.innerCol.rgba, innerRoundedRectAlpha).rgba * outerRoundedRectAlpha) * scissor;
    return result * strokeAlpha;
  }
  else if (uniforms.type == 0) {  // MNVG_SHADER_FILLGRAD
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy;
    float d = saturate((uniforms.feather * 0.5 + sdroundrect(pt, uniforms.extent, uniforms.radius))
                       / uniforms.feather);
    float4 color = mix(uniforms.innerCol, uniforms.outerCol, d);
    return color * scissor * strokeAlpha;
  } else if (uniforms.type == 1) {  // MNVG_SHADER_FILLIMG
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy / uniforms.extent;
    float4 color = texture.sample(sampler, pt);
    if (uniforms.texType == 1)
      color = float4(color.xyz * color.w, color.w);
    else if (uniforms.texType == 2)
      color = float4(color.x);
    color *= scissor;
    return color * uniforms.innerCol;
  } else if(uniforms.type == 5) {  // MNVG_SHADER_FILLCOLOR
      return uniforms.innerCol * strokeAlpha * scissor;
  }
  else {  // MNVG_SHADER_IMG
    float4 color = texture.sample(sampler, in.ftcoord);
    if (uniforms.texType == 1)
      color = float4(color.xyz * color.w, color.w);
    else if (uniforms.texType == 2)
      color = float4(color.x);
    color *= scissor;
    return color * uniforms.innerCol * strokeAlpha;
  }
}
// Fragment function (AA)
fragment float4 fragmentShaderAA(RasterizerData in [[stage_in]],
                                 constant Uniforms& uniforms [[buffer(0)]],
                                 texture2d<float> texture [[texture(0)]],
                                 sampler sampler [[sampler(0)]]) {
  float scissor = uniforms.scissorRadius == 0.0f ? scissorMask(uniforms, in.fpos) : roundedScissorMask(uniforms, in.fpos, uniforms.scissorRadius);
  if (scissor == 0) discard_fragment();

  if (uniforms.type == 2) {  // MNVG_SHADER_IMG
    float4 color = texture.sample(sampler, in.ftcoord);
    if (uniforms.texType == 1)
      color = float4(color.xyz * color.w, color.w);
    else if (uniforms.texType == 2)
      color = float4(color.x);
    color *= scissor;
    return color * uniforms.innerCol;
  }

  float strokeAlpha = strokeMask(uniforms, in.ftcoord);
  if (uniforms.lineStyle > 1 && strokeAlpha < uniforms.strokeThr) {
     discard_fragment();
  }
  if(uniforms.lineStyle == 2) strokeAlpha*=dashed(in.uv, uniforms.radius, uniforms.offset);
  if(uniforms.lineStyle == 3) strokeAlpha*=dotted(in.uv);
  if(uniforms.lineStyle == 4) strokeAlpha*=glow(in.uv);

  if (uniforms.type == 4) {  // MNVG_SHADER_FAST_ROUNDEDRECT
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy;
    float oD = sdroundrect(pt, uniforms.extent, uniforms.radius) - 0.04f;
	float outerD = fwidth(oD) * 0.5f;
	float iD = sdroundrect(pt, uniforms.extent - float2(1.0f), uniforms.radius - 1.0f) - 0.04f;
    float innerD = fwidth(iD) * 0.5f;
	float outerRoundedRectAlpha = clamp(inverseLerp(outerD, -outerD, oD), 0.0f, 1.0f);
    float innerRoundedRectAlpha = clamp(inverseLerp(innerD, -innerD, iD), 0.0f, 1.0f);
    float4 result = float4(mix(uniforms.outerCol.rgba, uniforms.innerCol.rgba, innerRoundedRectAlpha).rgba * outerRoundedRectAlpha) * scissor;
    return result * strokeAlpha;
  }

  if(uniforms.type == 6) { // MNVG_SHADER_DOUBLE_STROKE
    float colorMix = 1.0 - 2.15 * abs(in.uv.x);
    float4 icol = uniforms.innerCol;
    float smoothStart = 1.0f - uniforms.feather;
    float smoothEnd = uniforms.feather;
    if (in.uv.y < 0.0)
    {
        float dist = distance(2.0 * in.uv, float2(0.0, 0.0));
        strokeAlpha *= 1.0 - step(1.0, dist);
        float innerCap = 1.0 - smoothstep(smoothStart, smoothEnd, dist);
        icol = mix(uniforms.outerCol, icol, innerCap);
    }
    if (in.uv.y > uniforms.lineLength)
    {
        float2 capStart = float2(in.uv.x, (uniforms.lineLength - in.uv.y));
        float dist = distance(2.0 * capStart, float2(0.0, 0.0));
        strokeAlpha *= 1.0 - step(1.0, dist);
        float innerCap = 1.0 - smoothstep(smoothStart, smoothEnd, dist);
        icol = mix(uniforms.outerCol, icol, innerCap);
    }
    return mix(uniforms.outerCol, icol, smoothstep(smoothStart, smoothEnd, clamp(colorMix, 0.0, 1.0))) * strokeAlpha * scissor;
  }
  if(uniforms.type == 5) { // MNVG_SHADER_FILLCOLOR
      return uniforms.innerCol * strokeAlpha * scissor;
  }
  if(uniforms.type == 3) {
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy - (0.5 * uniforms.patternSize);
    float2 center = pt - float2(fmod(pt.x, uniforms.patternSize), fmod(pt.y, uniforms.patternSize)) + (0.5 * uniforms.patternSize);
    float circleDistVal = circleDist(pt, center, uniforms.radius);
    float4 dotColor = mix(uniforms.innerCol, uniforms.outerCol, smoothstep(0.5 - uniforms.feather, 0.5 + uniforms.feather, circleDistVal));
    float dotFade = 0.1 * distance(in.uv, float2(0.5, 0.5));
    float4 color = mix(dotColor, float4(0.0, 0.0, 0.0, 0.0), dotFade);
    color *= scissor;
    return color;
    }
  if (uniforms.type == 0) {  // MNVG_SHADER_FILLGRAD
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy;
    float d = saturate((uniforms.feather * 0.5 + sdroundrect(pt, uniforms.extent, uniforms.radius))
                        / uniforms.feather);
    float4 color = mix(uniforms.innerCol, uniforms.outerCol, d);
    color *= scissor;
    color *= strokeAlpha;
    return color;
  } else {  // MNVG_SHADER_FILLIMG
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy / uniforms.extent;
    float4 color = texture.sample(sampler, pt);
    if (uniforms.texType == 1)
      color = float4(color.xyz * color.w, color.w);
    else if (uniforms.texType == 2)
      color = float4(color.x);
    color *= scissor;
    color *= strokeAlpha;
    return color * uniforms.innerCol;
  }
}
