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
  float4 tcoord [[attribute(1)]];
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
  float4 dashCol;
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
  int reverse;
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

float gradientNoise(float2 uv){
    return fract(52.9829189 * fract(dot(uv, float2(0.06711056, 0.00583715))));
}

float4 normalBlend(float4 src, float4 dst) {
    float finalAlpha = src.a + dst.a * (1.0 - src.a);
    return float4((src.rgb * src.a + dst.rgb * dst.a * (1.0 - src.a)) / finalAlpha, finalAlpha);
}

float sigmoid(float t) {
    return 1.0 / (1.0 + exp(-t));
}

float sdSegment(float2 p, float2 a, float2 b ) {
    float2 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0f, 1.0f );
    return length( pa - ba*h );
}

float dashed(float2 uv, float rad, float thickness, float featherVal){
	float fy = fmod(uv.y, rad);
    float radThick = rad * .25f;
    float seg = sdSegment(float2(uv.x, fy), float2(0.0f, radThick + thickness), float2(0.0f, (rad * 0.5f) + radThick - thickness)) - thickness;
    float delta = fwidth(seg) * 0.5f;
    float aa = delta;
    float w = clamp(inverseLerp(aa, -aa, seg), 0.0f, 1.0f);
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

  out.ftcoord = vert.tcoord.xy * 2.0;
  out.uv = vert.tcoord.zw;
  out.fpos = vert.pos;
  out.pos = float4(2.0 * vert.pos.x / viewSize.x - 1.0,
                   1.0 - 2.0 * vert.pos.y / viewSize.y,
                   0, 1);
  return out;
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
  if(uniforms.lineStyle == 2) strokeAlpha*=dashed(float2(in.uv.x, in.uv.y * uniforms.lineLength - uniforms.offset), uniforms.radius, 0.45f, 0.0f);
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
    float revUVy = (uniforms.reverse > 0.5f) ? 0.5f - in.uv.y : in.uv.y;
    float2 uvLine = float2(in.uv.x, revUVy * uniforms.lineLength);
    float seg = sdSegment(uvLine, float2(0.0f), float2(0.0f, uniforms.lineLength * 0.5f));
    float outerSeg = seg - 0.45f;
    float outerDelta = fwidth(outerSeg);
    float outerShape = clamp(inverseLerp(outerDelta, -outerDelta, outerSeg), 0.0f, 1.0f);
    float innerSeg = seg - 0.22f;
    float innerDelta = fwidth(innerSeg);
    float innerShape = clamp(inverseLerp(innerDelta, -innerDelta, innerSeg), 0.0f, 1.0f);
    float pattern = 0.0f;
    if (uniforms.radius > 0.0f) {
        pattern = dashed(uvLine, uniforms.radius, 0.22f, uniforms.feather);
    }
    return mix(mix(uniforms.outerCol, uniforms.innerCol, smoothstep(0.0, 1.0, innerShape)), uniforms.dashCol, pattern * innerShape) * outerShape * scissor;
  }
  if(uniforms.type == 7) { // MNVG_SHADER_SMOOTH_GLOW
        float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0)).xy;
        float blurRadius = clamp(uniforms.radius, 2.0, 20.0) + uniforms.feather;
        float distShadow = clamp(sigmoid(sdroundrect(pt, uniforms.extent - float2(blurRadius), blurRadius) / uniforms.feather), 0.0, 1.0);
        float distRect = clamp(sdroundrect(pt, uniforms.extent - float2(5.5), uniforms.radius), 0.0, 1.0);
        float4 col = float4(uniforms.innerCol * (1.0 - distShadow));
        col = mix(float4(0.0), col, distRect);
        return col;
  }
  if(uniforms.type == 5) { // MNVG_SHADER_FILLCOLOR
      return uniforms.innerCol * strokeAlpha * scissor;
  }
  if(uniforms.type == 3) {
    float2 pt = (uniforms.paintMat * float3(in.fpos, 1.0f)).xy - (0.5f * uniforms.patternSize);
    float2 center = pt.xy - fmod(pt.xy, uniforms.patternSize) + (0.5f * uniforms.patternSize);
    float dist = circleDist(pt.xy, center, uniforms.radius);
    float delta = fwidth(dist);

    // We can use this variation for zoom >= 1.0f however, it may be fine as is on retina?
    //float alpha = smoothstep(0.45f - delta, 0.45f, dist);

	float alpha = smoothstep(uniforms.feather - delta, uniforms.feather + delta, dist);
    float4 dotColor = mix(uniforms.innerCol, uniforms.outerCol, alpha);
    return dotColor * scissor;
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
