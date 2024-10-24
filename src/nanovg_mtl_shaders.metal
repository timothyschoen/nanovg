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

typedef enum {
  MNVG_SHADER_FILLGRAD,
  MNVG_SHADER_FILLIMG,
  MNVG_SHADER_FILLIMG_ALPHA,
  MNVG_SHADER_IMG,
  MNVG_SHADER_FAST_ROUNDEDRECT,
  MNVG_SHADER_FILLCOLOR,
  MNVG_SHADER_DOUBLE_STROKE,
  MNVG_SHADER_SMOOTH_GLOW,
  MNVG_SHADER_DOUBLE_STROKE_GRAD,
  MNVG_SHADER_DOUBLE_STROKE_ACTIVITY,
  MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY,
  MNVG_SHADER_OBJECT_RECT,
} FragmentShaderCall;

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
  int type;
  int innerCol;
  int outerCol;
  int dashCol;
  float scissorMat[6];
  float paintMat[6];
  float2 scissorExt;
  float2 extent;
  float radius;
  float feather;
  float strokeMult;
  float scissorRadius;
  float offset;
  float lineLength;
  int stateData;
} Uniforms;

int getLineStyle(constant Uniforms& uniforms){
    return (uniforms.stateData >> 8) & 0x03;     // 2 bits
}
int getTexType(constant Uniforms& uniforms){
    return (uniforms.stateData >> 5) & 0x07;     // 3 bits (0,1,2,3,4)
}
bool getReverse(constant Uniforms& uniforms){
    return bool(uniforms.stateData & 0x01);      // 1 bit
}

float inverseLerp(float a, float b, float value) {
    return (value - a) / (b - a);
}

float3x3 transformInverse(const constant float t[6]) {
    float det = t[0] * t[3] + t[1] * -t[2];
    if(det == 0.0f) float3x3(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1));

    float invdet = 1.0f / det;
    return float3x3(float3(t[3], -t[1], 0.0f),
                    float3(-t[2], t[0], 0.0f),
                    float3(t[5] * t[2] - t[3] * t[4], -t[5] * t[0] + t[1] * t[4], t[3] * t[0] - t[1] * t[2])) * invdet;
}

float sdroundrect(float2 pt, float2 ext, float rad) {
	float2 ext2 = ext - float2(rad, rad);
	float2 d = abs(pt) - ext2;
	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;
}

float scissorMask(constant Uniforms& uniforms, float2 p) {
  float2 sc = (abs((transformInverse(uniforms.scissorMat) * float3(p,1.0f)).xy));
  float sc2 = sdroundrect(sc, uniforms.scissorExt, uniforms.scissorRadius) - 0.04f;
  float sc3 = fwidth(sc2) * 0.5;
  return clamp(inverseLerp(sc3, -sc3, sc2), 0.0f, 1.0f);
}

float sdSegment(float2 p, float2 a, float2 b ) {
    float2 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0f, 1.0f );
    return length( pa - ba*h );
}

float dashed(float2 uv, float rad, float thickness, float featherVal) {
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


float glow(float2 uv) {
  return smoothstep(0.0, 1.0, 1.0 - 2.0 * abs(uv.x));
}

float strokeMask(constant Uniforms& uniforms, thread RasterizerData& in) {
  float mask = min(1.0, (1.0 - abs(in.ftcoord.x * 2.0 - 1.0)) * uniforms.strokeMult) * min(1.0, in.ftcoord.y);
  if(getLineStyle(uniforms) == 2) mask*=dashed(float2(in.uv.x, in.uv.y * uniforms.lineLength - uniforms.offset), uniforms.radius, 0.45f, 0.0f);
  if(getLineStyle(uniforms) == 3) mask*=dotted(in.uv);
  if(getLineStyle(uniforms) == 4) mask*=glow(in.uv);
  return mask;
}


float capAlpha(float dist) {
  // Replace with your smoothstep function (consider basing it on line thickness)
  return smoothstep(0.4, 0.6, dist);
}

float circleDist(float2 p, float2 center, float d) {
  return distance(center, p) - d;
}

float4 getRawColour(int rgba){
    float4 col;
    col.b = float((rgba >> 24) & 0xFF) / 255.0f;
    col.g = float((rgba >> 16) & 0xFF) / 255.0f;
    col.r = float((rgba >> 8) & 0xFF) / 255.0f;
    col.a = float(rgba & 0xFF) / 255.0f;
    return col;
}

float4 convertColour(int rgba){
    float3 col;
    col.r = float((rgba >> 24) & 0xFF) / 255.0f;
    col.g = float((rgba >> 16) & 0xFF) / 255.0f;
    col.b = float((rgba >> 8) & 0xFF) / 255.0f;
    float a = float(rgba & 0xFF) / 255.0f;
    return float4((col * a).rgb, a);
}

float sigmoid(float t) {
    return 1.0 / (1.0 + exp(-t));
}

float2 rotatePoint(float2 p, float angle) {
    float cosAngle = cos(angle);
    float sinAngle = sin(angle);
    float2x2 rotationMatrix = float2x2(
        float2(cosAngle, -sinAngle),
        float2(sinAngle,  cosAngle)
    );
    return rotationMatrix * p;
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
  float scissor = scissorMask(uniforms, in.fpos);
  if (scissor == 0) discard_fragment();

  switch(uniforms.type)
  {
    case MNVG_SHADER_IMG: {
        float4 color = texture.sample(sampler, float2(in.ftcoord.x, getReverse(uniforms) ? 1.0f - in.ftcoord.y : in.ftcoord.y));
        if (getTexType(uniforms) == 1)
            color = float4(color.xyz * color.w, color.w);
        else if (getTexType(uniforms) == 2)
            color = float4(color.x);
        else if (getTexType(uniforms) == 3)
            color = color;
        color *= scissor;
        return (color * convertColour(uniforms.innerCol));
    }
    case MNVG_SHADER_FAST_ROUNDEDRECT:
    {
        float strokeAlpha = strokeMask(uniforms, in);
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos, 1.0)).xy;
        float oD = sdroundrect(pt, uniforms.extent, uniforms.radius) - 0.04f;
    	float outerD = fwidth(oD) * 0.5f;
    	float iD = sdroundrect(pt, uniforms.extent - float2(1.0f), uniforms.radius - 1.0f) - 0.04f;
        float innerD = fwidth(iD) * 0.5f;
    	float outerRoundedRectAlpha = clamp(inverseLerp(outerD, -outerD, oD), 0.0f, 1.0f);
        float innerRoundedRectAlpha = clamp(inverseLerp(innerD, -innerD, iD), 0.0f, 1.0f);
        float4 result = float4(mix(convertColour(uniforms.outerCol).rgba, convertColour(uniforms.innerCol).rgba, innerRoundedRectAlpha).rgba * outerRoundedRectAlpha) * scissor;
        return result * strokeAlpha;
    }
    case MNVG_SHADER_DOUBLE_STROKE:
    case MNVG_SHADER_DOUBLE_STROKE_GRAD:
    case MNVG_SHADER_DOUBLE_STROKE_ACTIVITY:
    case MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY:
    {
        // deal with path flipping here - instead of in geometry
        float revUVy = (getReverse(uniforms) > 0.5f) ? 0.5f - in.uv.y : in.uv.y;
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
        float activity = 0.0f;
        if (uniforms.type == MNVG_SHADER_DOUBLE_STROKE_ACTIVITY || uniforms.type == MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY) {
            activity = dashed(float2(uvLine.x, uvLine.y - (uniforms.offset * 3.0f)), 3.0f, 0.4f, uniforms.feather);
        }
        if (uniforms.type == MNVG_SHADER_DOUBLE_STROKE) {
            return mix(mix(convertColour(uniforms.outerCol), convertColour(uniforms.innerCol), smoothstep(0.0, 1.0, innerShape)), convertColour(uniforms.dashCol), pattern * innerShape) * outerShape;
        } else if (uniforms.type == MNVG_SHADER_DOUBLE_STROKE_ACTIVITY) {
            float4 overlay = mix(convertColour(uniforms.outerCol), float4(convertColour(uniforms.innerCol).rgb * 0.8f, 1.0f), activity);
            float4 mixedResult = mix(overlay, convertColour(uniforms.innerCol), innerShape);
            return mixedResult * outerShape;
        } else if (uniforms.type == MNVG_SHADER_DOUBLE_STROKE_GRAD || uniforms.type == MNVG_SHADER_DOUBLE_STROKE_GRAD_ACTIVITY) {
            float4 cable;
            if (uniforms.type == MNVG_SHADER_DOUBLE_STROKE_GRAD) {
                cable = mix(mix(convertColour(uniforms.outerCol), convertColour(uniforms.innerCol), smoothstep(0.0, 1.0, innerShape)), convertColour(uniforms.dashCol), pattern * innerShape);
            } else {
                float4 overlay = mix(convertColour(uniforms.outerCol), float4(convertColour(uniforms.innerCol).rgb * 0.8f, 1.0f), activity);
                float4 mixedResult = mix(overlay, convertColour(uniforms.innerCol), innerShape);
                cable = mixedResult * outerShape;
            }
            float scaledUV = in.uv.y * 2.0f * uniforms.lineLength;
            // Define the proportion of the line length where the fade should occur
            float fadeProportion = 0.3;

            // Calculate the fade range based on the line length, and make connections shorter than 60px solid
            float fadeRange = max(fadeProportion * uniforms.lineLength, 60.0f);

            float fade = smoothstep(0.4, fadeRange, scaledUV) * smoothstep(0.4, fadeRange, uniforms.lineLength - scaledUV);

            // limit fade transparency so it doesn't become fully transparent
            fade = min(fade, 0.7f);

            return (mix(cable, float4(0.0), fade)) * outerShape * scissor;
        }
    }
    case MNVG_SHADER_SMOOTH_GLOW: {
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos, 1.0)).xy;
        float blurRadius = clamp(uniforms.radius, 2.0, 20.0) + uniforms.feather;
        float distShadow = clamp(sigmoid(sdroundrect(pt, uniforms.extent - float2(blurRadius), blurRadius) / uniforms.feather), 0.0, 1.0);
        float distRect = clamp(sdroundrect(pt, uniforms.extent - float2(5.5), uniforms.radius), 0.0, 1.0);
        float4 col = float4(convertColour(uniforms.innerCol) * (1.0 - distShadow));
        col = mix(float4(0.0), col, distRect);
        return col;
    }
    case MNVG_SHADER_FILLCOLOR:
    {
        float strokeAlpha = strokeMask(uniforms, in);
        return convertColour(uniforms.innerCol) * strokeAlpha * scissor;
    }
    case MNVG_SHADER_OBJECT_RECT:
    {
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos,1.0f)).xy;
        int flagType = (uniforms.stateData >> 10) & 0x03;     // 2 bits

        float2 flagPoints[3];
        float flagSize = 5.0f;
        flagPoints[2] = float2(0.0f, -1.0f) * flagSize;

        bool objectOutline = bool((uniforms.stateData >> 12) & 0x01); // 1 bit (off or on)

        float offset = objectOutline ? 0.2f : -0.5f;

        float flag = 0.0f;
        switch (flagType){
            case 1: { // triangle flag top bottom
                flagPoints[1] = float2(-1.0f, -1.0f) * flagSize;
                float2 flagPosTopBottom = float2(pt.x, -abs(pt.y)) - float2(uniforms.extent.x + offset, -uniforms.extent.y);
                float2 rPoint = rotatePoint(flagPosTopBottom, 0.7854f); // 45 in radians
                flag = sdroundrect(rPoint, float2(flagSize), 0.0f);
                break;
            }
            case 2: { // triangle flag top only
                flagPoints[1] = float2(-1.0f, -1.0f) * flagSize;

                float2 flagPosTop = pt - float2(uniforms.extent.x + offset, -uniforms.extent.y);
                float2 rPoint2 = rotatePoint(flagPosTop, 0.7854f); // 45 in radians
                flag = sdroundrect(rPoint2, float2(flagSize), 0.0f);
                break;
            }
            case 3: { // composite square & triangle top / bottom
                flagSize = 3.5f;
                flagPoints[1] = float2(-1.0f, 0.0f) * flagSize;

                float hypot = length(float2(flagSize));
                float2 messageFlag = float2(pt.x, -abs(pt.y)) - float2(uniforms.extent.x + offset, -uniforms.extent.y + hypot);
                float2 rPoint3 = rotatePoint(messageFlag, 0.7854f); // 45 in radians
                float triangle = sdroundrect(rPoint3, float2(flagSize), 0.0f);
                float squareMid = (uniforms.extent.y - flagSize) * 0.5f;
                float square = sdroundrect(float2(messageFlag.x, messageFlag.y - squareMid), float2(hypot, squareMid), 0.0f);
                flag = min(triangle, square); // union
                break;
            }
            default:
                break;
            }

        float oD = sdroundrect(pt, uniforms.extent, uniforms.radius) - 0.04f; // Calculate outer rectangle

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

        float4 finalColor;
        if (objectOutline) {
            finalColor = mix(convertColour(uniforms.outerCol), convertColour(uniforms.innerCol), innerRoundedRectAlpha);
        } else {
            finalColor = mix(convertColour(uniforms.outerCol), mix(convertColour(uniforms.innerCol), convertColour(uniforms.dashCol), triFlagShape), innerRoundedRectAlpha);
        }

        return float4(finalColor * outerRoundedRectAlpha) * scissor;
    }
    case MNVG_SHADER_FILLGRAD: {
        float strokeAlpha = strokeMask(uniforms, in);
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos, 1.0)).xy;
        float d = saturate((uniforms.feather * 0.5 + sdroundrect(pt, uniforms.extent, uniforms.radius))
                            / uniforms.feather);
        float4 color = mix(convertColour(uniforms.innerCol), convertColour(uniforms.outerCol), d);
        color *= scissor;
        color *= strokeAlpha;
        return color;
    }
    case MNVG_SHADER_FILLIMG_ALPHA: {
        // Calculate alpha from texture
        float strokeAlpha = strokeMask(uniforms, in);
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos, 1.0)).xy / uniforms.extent;
        float4 color = texture.sample(sampler, float2(pt.x, getReverse(uniforms) ? 1.0f - pt.y : pt.y));
        float alpha = color.a;
        if (getTexType(uniforms) == 1) alpha = color.w;
        if (getTexType(uniforms) == 2) alpha = color.x;
        if (getTexType(uniforms) == 4) alpha = color.r; // single channel GL_RED
        // Apply color tint and alpha.
        float3 maskColor = getRawColour(uniforms.innerCol).bgr;
        return float4(maskColor * alpha, alpha) * strokeAlpha * scissor;
    }
    case MNVG_SHADER_FILLIMG:
    {
        float strokeAlpha = strokeMask(uniforms, in);
        float2 pt = (transformInverse(uniforms.paintMat) * float3(in.fpos, 1.0)).xy / uniforms.extent;
        float4 color = texture.sample(sampler, float2(pt.x, getReverse(uniforms) ? 1.0f - pt.y : pt.y));
        if (getTexType(uniforms) == 1) color = float4(color.xyz * color.w, color.w);
        else if (getTexType(uniforms) == 2) color = float4(color.x);
        else if (getTexType(uniforms) == 3) color = color;
        return (color * scissor * strokeAlpha).rgba;
    }
  }

  return 0;
}
