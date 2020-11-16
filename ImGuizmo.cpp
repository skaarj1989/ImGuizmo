// The MIT License(MIT)
//
// Copyright(c) 2016 Cedric Guillemet
// Copyright(c) 2020 Dawid Kurek
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files(the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions :
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#ifndef IMGUI_DEFINE_MATH_OPERATORS
#  define IMGUI_DEFINE_MATH_OPERATORS
#endif
#include "ImGuizmo.h"
#include "imgui_internal.h"

#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtx/common.hpp"
#include "glm/gtx/compatibility.hpp"
#include "glm/gtx/transform.hpp"

/*
  @todo
  - dots at the end of scale axes
  - gizmo flags (visibility during operation/drag, lock axes ...)
*/

//-----------------------------------------------------------------------------
// [SECTION] CONSTANTS
//-----------------------------------------------------------------------------

constexpr auto kPi = glm::pi<float>();
constexpr auto kEpsilon = glm::epsilon<float>();

const glm::vec3 kReferenceUp{ 0.0f, 1.0f, 0.0f };
const glm::vec3 kUnitDirections[3]{
  { 1.0f, 0.0f, 0.0f }, // Right
  { 0.0f, 1.0f, 0.0f }, // Up
  { 0.0f, 0.0f, 1.0f }  // Forward
};

// Size of the quads responsible for movement on a plane
constexpr float kQuadSize{ 0.20f };
constexpr float kQuadMin{ 0.30f };
constexpr float kQuadMax{ kQuadMin + kQuadSize };
const float kUnitQuad[]{ kQuadMin, kQuadMin, kQuadMin, kQuadMax,
                         kQuadMax, kQuadMax, kQuadMax, kQuadMin };

constexpr float kRotationRingScale{ 0.06f };
constexpr float kCircleRadius{ 6.0f };  // Translation and scale dots
constexpr float kLineThickness{ 3.0f }; // Translation and scale axes
constexpr int kArcSegmentCount{ 64 };

//-----------------------------------------------------------------------------
// [SECTION] INTERNAL TYPES
//-----------------------------------------------------------------------------

enum ImGuizmoAxisFlags_ {
  ImGuizmoAxisFlags_None = 0,

  ImGuizmoAxisFlags_X = 1 << 0,
  ImGuizmoAxisFlags_Y = 1 << 1,
  ImGuizmoAxisFlags_Z = 1 << 2,

  ImGuizmoAxisFlags_YZ = ImGuizmoAxisFlags_Y | ImGuizmoAxisFlags_Z,
  ImGuizmoAxisFlags_ZX = ImGuizmoAxisFlags_Z | ImGuizmoAxisFlags_X,
  ImGuizmoAxisFlags_XY = ImGuizmoAxisFlags_X | ImGuizmoAxisFlags_Y,

  ImGuizmoAxisFlags_ALL =
    ImGuizmoAxisFlags_X | ImGuizmoAxisFlags_Y | ImGuizmoAxisFlags_Z
};
using ImGuizmoMoveDirection = int;

enum ImGuizmoAxis_ {
  ImGuizmoAxis_X,
  ImGuizmoAxis_Y,
  ImGuizmoAxis_Z,

  ImGuizmoAxis_COUNT
};
using ImGuizmoAxis = int;

bool IsAxisIdxValid(ImGuizmoAxis axisIdx) {
  return axisIdx >= 0 && axisIdx < ImGuizmoAxis_COUNT;
}
bool HasSingleAxis(ImGuizmoMoveDirection flags) {
  switch (flags) {
  case ImGuizmoAxisFlags_X:
  case ImGuizmoAxisFlags_Y:
  case ImGuizmoAxisFlags_Z:
    return true;

  default:
    return false;
  }
}

ImGuizmoAxis GetAxisIdx(ImGuizmoMoveDirection flags) {
  IM_ASSERT(HasSingleAxis(flags));
  switch (flags) {
  case ImGuizmoAxisFlags_X:
    return ImGuizmoAxis_X;
  case ImGuizmoAxisFlags_Y:
    return ImGuizmoAxis_Y;
  case ImGuizmoAxisFlags_Z:
    return ImGuizmoAxis_Z;

  default:
    return -1;
  }
}
ImGuizmoMoveDirection AxisToFlag(ImGuizmoAxis axisIdx) {
  IM_ASSERT(IsAxisIdxValid(axisIdx));

  switch (axisIdx) {
  case ImGuizmoAxis_X:
    return ImGuizmoAxisFlags_X;
  case ImGuizmoAxis_Y:
    return ImGuizmoAxisFlags_Y;
  case ImGuizmoAxis_Z:
    return ImGuizmoAxisFlags_Z;

  default:
    return -1;
  }
}

ImGuizmoAxis GetAxisAroundIdx(ImGuizmoAxis axisIdx) {
  IM_ASSERT(IsAxisIdxValid(axisIdx));
  switch (axisIdx) {
  case ImGuizmoAxis_X:
    return ImGuizmoAxis_Z;
  case ImGuizmoAxis_Y:
    return ImGuizmoAxis_Y;
  case ImGuizmoAxis_Z:
    return ImGuizmoAxis_X;

  default:
    return -1;
  }
}
ImGuizmoAxis GetScaleAxisIdx(ImGuizmoAxis axisIdx) {
  IM_ASSERT(IsAxisIdxValid(axisIdx));
  switch (axisIdx) {
  case ImGuizmoAxis_X:
    return ImGuizmoAxis_Y;
  case ImGuizmoAxis_Y:
    return ImGuizmoAxis_Z;
  case ImGuizmoAxis_Z:
    return ImGuizmoAxis_X;

  default:
    return -1;
  }
}

enum ImGuizmoPlane_ {
  ImGuizmoPlane_YZ,
  ImGuizmoPlane_ZX,
  ImGuizmoPlane_XY,

  ImGuizmoPlane_COUNT
};
using ImGuizmoPlane = int;

bool IsPlaneIdxValid(ImGuizmoPlane planeIdx) {
  return planeIdx >= 0 && planeIdx < ImGuizmoPlane_COUNT;
}
bool HasPlane(ImGuizmoMoveDirection flags) {
  switch (flags) {
  case ImGuizmoAxisFlags_YZ:
  case ImGuizmoAxisFlags_ZX:
  case ImGuizmoAxisFlags_XY:
    return true;

  default:
    return false;
  }
}
ImGuizmoPlane GetPlaneIdx(ImGuizmoMoveDirection flags) {
  IM_ASSERT(HasPlane(flags));

  switch (flags) {
  case ImGuizmoAxisFlags_YZ:
    return ImGuizmoPlane_YZ;
  case ImGuizmoAxisFlags_ZX:
    return ImGuizmoPlane_ZX;
  case ImGuizmoAxisFlags_XY:
    return ImGuizmoPlane_XY;

  default:
    return -1;
  }
}
ImGuizmoMoveDirection PlaneToFlags(ImGuizmoPlane planeIdx) {
  IM_ASSERT(IsPlaneIdxValid(planeIdx));

  switch (planeIdx) {
  case ImGuizmoPlane_YZ:
    return ImGuizmoAxisFlags_YZ;
  case ImGuizmoPlane_ZX:
    return ImGuizmoAxisFlags_ZX;
  case ImGuizmoPlane_XY:
    return ImGuizmoAxisFlags_XY;

  default:
    return -1;
  }
}

//-----------------------------------------------------------------------------
// [SECTION] TYPES
//-----------------------------------------------------------------------------

struct ImGuizmoRay {
  glm::vec3 Start{ 0.0f };
  glm::vec3 End{ 0.0f };
  glm::vec3 Direction{ 0.0f };
};

struct ImGuizmoCamera {
  bool IsOrtho{ false };

  glm::mat4 ViewMatrix{ 1.0f };
  glm::mat4 ProjectionMatrix{ 1.0f };
  glm::mat4 ViewProjectionMatrix{ 1.0f };

  glm::vec3 Right{ kUnitDirections[0] };
  glm::vec3 Up{ kUnitDirections[1] };
  glm::vec3 Forward{ kUnitDirections[2] };
  glm::vec3 Eye{ 0.0f };
};

struct ImGuizmoWidget {
  ImGuiID ID{ 0 };

  // Used as reference model matrix, doesn't change while manipulating
  glm::mat4 SourceModelMatrix{ 1.0f };
  glm::mat4 ModelMatrix{ 1.0f };
  glm::mat4 InversedModelMatrix{ 1.0f };

  glm::mat4 ModelViewProjMatrix{ 1.0f };

  ImGuizmoMode Mode{ ImGuizmoMode_Local };
  // Translate/ Rotate/ Scale
  ImGuizmoOperation ActiveOperation{ ImGuizmoOperation_None };
  ImGuizmoMoveDirection ActiveManipulation{ ImGuizmoAxisFlags_None };
  bool Dirty{ false };

  // Screen space values
  glm::vec2 Origin{ 0.0f };
  float RingRadius{ 0.0f };
  float ScreenFactor{ 0.0f };

  // Shared across transformations
  glm::vec4 TranslationPlane{ 0.0f };       // T+R+S
  glm::vec3 TranslationPlaneOrigin{ 0.0f }; // T+S
  glm::vec3 ModelRelativeOrigin{ 0.0f };    // T+S
  glm::vec3 DragTranslationOrigin{ 0.0f };  // T+S

  // Translation
  glm::vec3 LastTranslationDelta{ 0.0f };

  // Rotation
  glm::vec3 ModelScaleOrigin{ 1.0f };
  glm::vec3 RotationVectorSource{ 0.0f };
  float RotationAngle{ 0.0f };       // In radians
  float RotationAngleOrigin{ 0.0f }; // In radians

  // Scale
  glm::vec3 Scale{ 1.0f };
  glm::vec3 LastScale{ 1.0f };
  glm::vec3 ScaleValueOrigin{ 1.0f };

  // ---

  explicit ImGuizmoWidget(ImGuiID id) : ID{ id } {}

  float ComputeAngleOnPlane() const;
};

struct ImGuizmoCage {
  bool InUse{ false };

  glm::vec3 LocalPivot{ 0.0f };
  glm::vec3 Pivot{ 0.0f };
  glm::vec3 Anchor{ 0.0f };
  glm::vec4 Plane{ 0.0f };
  int BestAxis{ 0 };
  int Axis[2]{ 0 };
  glm::mat4 Matrix{ 1.0f };
};

//-----------------------------------------------------------------------------
// [SECTION] CONTEXT
//-----------------------------------------------------------------------------

struct ImGuizmoContext {
  ImDrawList *DrawList{ nullptr };

  ImGuizmoStyle Style;

  ImRect Viewport;
  ImGuizmoCamera Camera;
  ImGuizmoRay Ray;
  glm::vec2 DragOrigin{ 0.0f };

  ImVector<ImGuizmoWidget *> Gizmos;
  ImGuiStorage GizmosById;

  ImGuizmoWidget *CurrentGizmo{ nullptr }; // Gizmo in Begin/End scope
  ImGuizmoWidget *ActiveGizmo{ nullptr };  // Currently manipulated gizmo

  ImGuizmoCage Bounds;

  float *LockedModelMatrix{ nullptr };

  // ---

  ImGuizmoContext() = default;
  ~ImGuizmoContext();

  float GetAspectRatio() const;
};
static ImGuizmoContext GImGuizmo;

ImGuizmoWidget *FindGizmoById(ImGuiID id) {
  const ImGuizmoContext &g{ GImGuizmo };
  return static_cast<ImGuizmoWidget *>(g.GizmosById.GetVoidPtr(id));
}
ImGuizmoWidget *CreateNewGizmo(ImGuiID id) {
  ImGuizmoContext &g{ GImGuizmo };
  auto *gizmo{ new ImGuizmoWidget(id) };
  g.GizmosById.SetVoidPtr(id, gizmo);
  g.Gizmos.push_back(gizmo);
  return gizmo;
}
ImGuizmoWidget *GetCurrentGizmo() { return GImGuizmo.CurrentGizmo; }

ImGuizmoStyle::ImGuizmoStyle() { ImGuizmo::StyleColorsDefault(this); }

namespace ImGuizmo {

//-----------------------------------------------------------------------------
// [SECTION] STYLING
//-----------------------------------------------------------------------------

ImGuizmoStyle &GetStyle() { return GImGuizmo.Style; }
void StyleColorsDefault(ImGuizmoStyle *dst) {
  ImGuizmoStyle *style{ dst ? dst : &ImGuizmo::GetStyle() };
  ImVec4 *colors{ style->Colors };

  // auto col = ImGui::ColorConvertU32ToFloat4(0x801080FF);

  colors[ImGuizmoCol_Inactive] = ImVec4{ 0.600f, 0.600f, 0.600f, 0.600f };
  colors[ImGuizmoCol_Selection] = ImVec4{ 1.00f, 0.501f, 0.062f, 1.00f };

  colors[ImGuizmoCol_SpecialMove] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };

  colors[ImGuizmoCol_Text] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };
  colors[ImGuizmoCol_TextShadow] = ImVec4{ 0.00f, 0.00f, 0.00f, 1.00f };

  colors[ImGuizmoCol_AxisX] = ImVec4{ 0.764f, 0.156f, 0.117f, 1.00f };
  colors[ImGuizmoCol_AxisY] = ImVec4{ 0.411f, 0.686f, 0.098f, 1.00f };
  colors[ImGuizmoCol_AxisZ] = ImVec4{ 0.215f, 0.509f, 0.823f, 1.00f };

  colors[ImGuizmoCol_PlaneYZ] = ImVec4{ 0.764f, 0.156f, 0.117f, 0.60f };
  colors[ImGuizmoCol_PlaneZX] = ImVec4{ 0.411f, 0.686f, 0.098f, 0.60f };
  colors[ImGuizmoCol_PlaneXY] = ImVec4{ 0.215f, 0.509f, 0.823f, 0.60f };
}

static ImU32 GetColorU32(ImGuiCol idx, float alpha_mul = 1.0f) {
  const ImGuizmoStyle &style{ GImGuizmo.Style };
  ImVec4 c{ style.Colors[idx] };
  c.w *= style.Alpha * alpha_mul;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static const ImVec4 &GetStyleColorVec4(ImGuiCol idx) {
  const ImGuizmoStyle &style{ GImGuizmo.Style };
  return style.Colors[idx];
}

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES (Geometry functions)
//-----------------------------------------------------------------------------

static glm::vec2 WorldToScreen(const glm::vec3 &worldPos,
                               const glm::mat4 &matrix,
                               const glm::vec2 &position,
                               const glm::vec2 &size) {
  glm::vec4 temp{ matrix * glm::vec4{ worldPos, 1.0f } };
  temp *= 0.5f / temp.w;

  glm::vec2 screenPos{ temp.xy() + 0.5f };
  screenPos.y = 1.0f - screenPos.y;
  screenPos *= size;
  screenPos += position;
  return screenPos;
}
static glm::vec2 WorldToScreen(const glm::vec3 &worldPos,
                               const glm::mat4 &matrix) {
  const ImGuizmoContext &g{ GImGuizmo };
  return WorldToScreen(worldPos, matrix, g.Viewport.GetTL(),
                       g.Viewport.GetSize());
}

static ImGuizmoRay RayCast(const glm::mat4 &viewProjMatrix,
                           const glm::vec2 &position, const glm::vec2 &size) {
  // Convert to NDC
  glm::vec2 mousePos{ ImGui::GetIO().MousePos };
  mousePos = ((mousePos - position) / size) * 2.0f - 1.0f;
  mousePos.y *= -1.0f;

  const glm::mat4 inversedViewProj{ glm::inverse(viewProjMatrix) };
  glm::vec4 rayStartWorldSpace{ inversedViewProj *
                                glm::vec4{ mousePos, 0.0f, 1.0f } };
  rayStartWorldSpace *= 1.0f / rayStartWorldSpace.w;
  glm::vec4 rayEndWorldSpace{ inversedViewProj *
                              glm::vec4{ mousePos, 1.0f - kEpsilon, 1.0f } };
  rayEndWorldSpace *= 1.0f / rayEndWorldSpace.w;
  return ImGuizmoRay{ rayStartWorldSpace, rayEndWorldSpace,
                      glm::normalize(rayEndWorldSpace - rayStartWorldSpace) };
}
static ImGuizmoRay RayCast(const glm::mat4 &viewProjMatrix) {
  const ImGuizmoContext &g{ GImGuizmo };
  return RayCast(viewProjMatrix, g.Viewport.GetTL(), g.Viewport.GetSize());
}

static float GetParallelogram(const glm::vec4 &ptO, const glm::vec4 &ptA,
                              const glm::vec4 &ptB) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  glm::vec4 points[]{ ptO, ptA, ptB };
  for (int i = 0; i < 3; ++i) {
    points[i].w = 1.0f;
    points[i] = gizmo->ModelViewProjMatrix * points[i];
    // Check for axis aligned with camera direction
    if (glm::abs(points[i].w) > kEpsilon) {
      points[i] *= 1.0f / points[i].w;
    }
  }

  glm::vec2 segA{ points[1] - points[0] };
  segA.y /= g.GetAspectRatio();
  glm::vec2 segB{ points[2] - points[0] };
  segB.y /= g.GetAspectRatio();

  const auto segAOrtho = glm::normalize(glm::vec2{ -segA.y, segA.x });
  const float dt{ glm::dot(segAOrtho, segB) };
  const float surface{ glm::length(segA) * glm::abs(dt) };
  return surface;
}

static glm::vec4 BuildPlane(const glm::vec3 &point, const glm::vec3 &normal) {
  const glm::vec3 n{ glm::normalize(normal) };
  return glm::vec4{ n, glm::dot(n, point) };
}
static float DistanceToPlane(const glm::vec3 &point, const glm::vec4 &plane) {
  return glm::dot(plane.xyz(), point) + plane.w;
}
static float IntersectRayPlane(const ImGuizmoRay &ray, const glm::vec4 &plane) {
  const float num{ glm::dot(plane.xyz(), ray.Start) - plane.w };
  const float denom{ glm::dot(plane.xyz(), ray.Direction) };

  // Normal is orthogonal to vector, cantt intersect
  if (glm::abs(denom) < kEpsilon) return -1.0f;
  return -(num / denom);
}

static glm::vec2 PointOnSegment(const glm::vec2 &point, const glm::vec2 &v1,
                                const glm::vec2 &v2) {
  const glm::vec2 c{ point - v1 };
  const glm::vec2 V{ glm::normalize(v2 - v1) };
  const float d{ glm::length(v2 - v1) };
  const float t{ glm::dot(V, c) };

  if (t < 0) return v1;
  if (t > d) return v2;

  return v1 + V * t;
}
static float GetSegmentLengthClipSpace(const glm::vec3 &start,
                                       const glm::vec3 &end) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  auto startOfSegment = gizmo->ModelViewProjMatrix * glm::vec4{ start, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(startOfSegment.w) > kEpsilon)
    startOfSegment *= 1.0f / startOfSegment.w;

  auto endOfSegment = gizmo->ModelViewProjMatrix * glm::vec4{ end, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(endOfSegment.w) > kEpsilon)
    endOfSegment *= 1.0f / endOfSegment.w;

  glm::vec2 clipSpaceAxis{ endOfSegment - startOfSegment };
  clipSpaceAxis.y /= g.GetAspectRatio();
  return glm::length(clipSpaceAxis);
}

//-----------------------------------------------------------------------------
// [SECTION] UTILITIES (Snap)
//-----------------------------------------------------------------------------

static void ComputeSnap(float &value, float snap) {
  if (snap <= kEpsilon) return;

  const float modulo{ glm::fmod(value, snap) };
  const float moduloRatio{ glm::abs(modulo) / snap };
  constexpr float kSnapTension{ 0.5f };
  if (moduloRatio < kSnapTension) {
    value -= modulo;
  } else if (moduloRatio > (1.0f - kSnapTension)) {
    value = value - modulo + snap * ((value < 0.0f) ? -1.0f : 1.0f);
  }
}
static void ComputeSnap(glm::vec3 &value, const float *snap) {
  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
    ComputeSnap(value[axis], snap[axis]);
}

//-----------------------------------------------------------------------------
// [SECTION] UTILITIES (Text)
//-----------------------------------------------------------------------------

static void RenderText(const glm::vec2 &position, const char *text) {
  const ImGuizmoContext &g{ GImGuizmo };
  g.DrawList->AddText(position + 15.0f, GetColorU32(ImGuizmoCol_TextShadow),
                      text);
  g.DrawList->AddText(position + 14.0f, GetColorU32(ImGuizmoCol_Text), text);
}

const char *kInfoMasks[]{
  // -- Translation:
  "X : %5.3f",                     // 0
  "Y : %5.3f",                     // 1
  "Z : %5.3f",                     // 2
  "Y : %5.3f Z : %5.3f",           // 3
  "X : %5.3f Z : %5.3f",           // 6
  "X : %5.3f Y : %5.3f",           // 9
  "X : %5.3f Y : %5.3f Z : %5.3f", // 0

  // -- Rotation:
  "X : %5.2f deg %5.2f rad",      // 0
  "Y : %5.2f deg %5.2f rad",      // 1
  "Z : %5.2f deg %5.2f rad",      // 2
  "Screen : %5.2f deg %5.2f rad", // 0

  // -- Scale
  "XYZ : %5.2f" // 0
};
const int kInfoIndices[]{
  0, 1, 2, // XYZ
  1, 2, 0, // YZ (0-unused)
  0, 2, 0, // XZ (0-unused)
  0, 1, 0, // XY (0-unused)
};

void RenderTranslationInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection &flags{ gizmo->ActiveManipulation };
  const char *mask{ nullptr };
  int startIdx{ 0 };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axisIdx{ GetAxisIdx(flags) };
    mask = kInfoMasks[axisIdx];
    startIdx = axisIdx;
  } else if (HasPlane(flags)) {
    const ImGuizmoPlane planeIdx{ GetPlaneIdx(flags) };
    mask = kInfoMasks[ImGuizmoAxis_COUNT + planeIdx];
    startIdx = ImGuizmoAxis_COUNT + (ImGuizmoPlane_COUNT * planeIdx);
  } else {
    mask = kInfoMasks[ImGuizmoAxis_COUNT + ImGuizmoPlane_COUNT];
  }

  const glm::vec3 deltaInfo{ gizmo->ModelMatrix[3].xyz() -
                             gizmo->DragTranslationOrigin };

  char infoBuffer[128]{};
  ImFormatString(infoBuffer, sizeof(infoBuffer), mask,
                 deltaInfo[kInfoIndices[startIdx]],
                 deltaInfo[kInfoIndices[startIdx + 1]],
                 deltaInfo[kInfoIndices[startIdx + 2]]);
  RenderText(gizmo->Origin, infoBuffer);
}
void RenderRotationInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection &flags{ gizmo->ActiveManipulation };
  const char *mask{ nullptr };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axisIdx{ (GetAxisIdx(flags)) };
    mask = kInfoMasks[ImGuizmoAxis_COUNT + ImGuizmoPlane_COUNT + 1 + axisIdx];
  } else {
    mask = kInfoMasks[(ImGuizmoAxis_COUNT * 2) + ImGuizmoPlane_COUNT];
  }

  char infoBuffer[128]{};
  ImFormatString(infoBuffer, sizeof(infoBuffer), mask,
                 glm::degrees(gizmo->RotationAngle), gizmo->RotationAngle);
  RenderText(gizmo->Origin, infoBuffer);
}
void RenderScaleInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection &flags{ gizmo->ActiveManipulation };
  const char *mask{ nullptr };
  int startIdx{ 0 };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axisIdx{ GetAxisIdx(flags) };
    mask = kInfoMasks[axisIdx];
    startIdx = axisIdx;
  }
  else {
    mask = kInfoMasks[11];
  }

  char infoBuffer[128]{};
  ImFormatString(infoBuffer, sizeof(infoBuffer), mask,
                 gizmo->Scale[kInfoIndices[startIdx]],
                 gizmo->Scale[kInfoIndices[startIdx + 1]],
                 gizmo->Scale[kInfoIndices[startIdx + 2]]);
  RenderText(gizmo->Origin, infoBuffer);
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

//-----------------------------------------------------------------------------
// [SECTION] 
//-----------------------------------------------------------------------------

void FeedGizmo(const float *model) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->SourceModelMatrix = glm::make_mat4(model);
  if (gizmo->Mode == ImGuizmoMode_Local) {
    gizmo->ModelMatrix = gizmo->SourceModelMatrix;
    for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
      gizmo->ModelMatrix[axis] = glm::normalize(gizmo->ModelMatrix[axis]);
  } else {
    gizmo->ModelMatrix =
      glm::translate(glm::vec3{ gizmo->SourceModelMatrix[3] });
  }
  gizmo->ModelViewProjMatrix =
    g.Camera.ViewProjectionMatrix * gizmo->ModelMatrix;

  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
    gizmo->ModelScaleOrigin[axis] = glm::length(gizmo->SourceModelMatrix[axis]);

  gizmo->Origin = WorldToScreen(glm::vec3{ 0.0f }, gizmo->ModelViewProjMatrix);
  // gizmo->RingRadius = kRotationRingScale * g.Viewport.GetHeight();

  gizmo->InversedModelMatrix = glm::inverse(gizmo->ModelMatrix);
  const glm::vec3 rightViewInverse{ gizmo->InversedModelMatrix *
                                    glm::vec4{ g.Camera.Right, 0.0f } };
  const float rightLength{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                     rightViewInverse) };
  gizmo->ScreenFactor = g.Style.GizmoScale / rightLength;
}
bool GizmoBehavior(ImGuizmoOperation operation, ImGuizmoMoveDirection &hovered,
                   bool *out_held) {
  IM_ASSERT(operation != ImGuizmoOperation_None);

  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (g.ActiveGizmo != gizmo && g.ActiveGizmo != nullptr) {
    hovered = ImGuizmoAxisFlags_None;
  } else {
    if (gizmo->ActiveOperation != operation &&
        gizmo->ActiveOperation != ImGuizmoOperation_None) {
      hovered = ImGuizmoAxisFlags_None;
    } else {
      if (gizmo->ActiveManipulation != ImGuizmoAxisFlags_None)
        hovered = gizmo->ActiveManipulation;
    }
  }

  bool pressed{ hovered != ImGuizmoAxisFlags_None && io.MouseClicked[0] };
  if (pressed) {
    g.ActiveGizmo = gizmo;
    gizmo->ActiveOperation = operation;
    gizmo->ActiveManipulation = hovered;
  }

  bool held{ false };
  if (gizmo->ActiveManipulation != ImGuizmoAxisFlags_None &&
      gizmo->ActiveManipulation == hovered) {
    if (io.MouseDown[0]) {
      held = true;
    } else {
      g.ActiveGizmo = nullptr;
      gizmo->ActiveManipulation = ImGuizmoAxisFlags_None;
    }
  }

  if (out_held) *out_held = held;
  return pressed;
}

void SetViewport(const ImRect &viewport) { GImGuizmo.Viewport = viewport; }

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

bool MouseOverOrigin() {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if constexpr (true) { // Origin as circle
    const float distance{ glm::length(glm::vec2{ io.MousePos } -
                                      gizmo->Origin) };
    return distance <= kCircleRadius;
  } else { // square
    constexpr float kSize{ kCircleRadius };
    ImRect bb{ gizmo->Origin - kSize, gizmo->Origin + kSize };
    return bb.Contains(io.MousePos);
  }
}
bool MouseOverAxis(ImGuizmoAxis axisIdx) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const glm::vec3 dirAxis{ gizmo->ModelMatrix *
                           glm::vec4{ kUnitDirections[axisIdx], 0.0f } };
  const float length{ IntersectRayPlane(
    g.Ray, BuildPlane(gizmo->ModelMatrix[3], dirAxis)) };
  const glm::vec2 mousePosOnPlane{ WorldToScreen(
    g.Ray.Start + g.Ray.Direction * length, g.Camera.ViewProjectionMatrix) };

  constexpr float kAxisShift{ 0.1f };
  const glm::vec2 axisStartOnScreen{ WorldToScreen(
    gizmo->ModelMatrix[3].xyz() + dirAxis * gizmo->ScreenFactor * kAxisShift,
    g.Camera.ViewProjectionMatrix) };
  const glm::vec2 axisEndOnScreen{ WorldToScreen(
    gizmo->ModelMatrix[3].xyz() + dirAxis * gizmo->ScreenFactor,
    g.Camera.ViewProjectionMatrix) };

  const glm::vec2 closestPointOnAxis{ PointOnSegment(
    mousePosOnPlane, axisStartOnScreen, axisEndOnScreen) };
  constexpr float kTolerance{ 6.0f };
  return glm::length(closestPointOnAxis - mousePosOnPlane) < kTolerance;
}
bool MouseOverPlane(ImGuizmoPlane planeIdx) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const glm::vec3 dirAxis{ gizmo->ModelMatrix *
                           glm::vec4{ kUnitDirections[planeIdx], 0.0f } };
  const float length{ IntersectRayPlane(
    g.Ray, BuildPlane(gizmo->ModelMatrix[3], dirAxis)) };
  const glm::vec3 mousePosOnPlane{ g.Ray.Start + g.Ray.Direction * length };

  const glm::vec3 planeDir1{
    gizmo->ModelMatrix * glm::vec4{ kUnitDirections[(planeIdx + 1) % 3], 0.0f }
  };
  const float dx{ glm::dot(planeDir1,
                           (mousePosOnPlane - gizmo->ModelMatrix[3].xyz()) *
                             (1.0f / gizmo->ScreenFactor)) };
  const glm::vec3 planeDir2{
    gizmo->ModelMatrix * glm::vec4{ kUnitDirections[(planeIdx + 2) % 3], 0.0f }
  };
  const float dy{ glm::dot(planeDir2,
                           (mousePosOnPlane - gizmo->ModelMatrix[3].xyz()) *
                             (1.0f / gizmo->ScreenFactor)) };

  return (dx >= kUnitQuad[0] && dx <= kUnitQuad[4] && dy >= kUnitQuad[1] &&
          dy <= kUnitQuad[3]);
}

//-----------------------------------------------------------------------------
// [SECTION] TRANSLATION
//-----------------------------------------------------------------------------

glm::vec4 BuildTranslatePlane() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };

  glm::vec3 movePlaneNormal;
  if (HasPlane(flags)) {
    movePlaneNormal = gizmo->ModelMatrix[GetPlaneIdx(flags)];
  } else if (HasSingleAxis(flags)) {
    const glm::vec3 dir{ gizmo->ModelMatrix[GetAxisIdx(flags)] };
    const glm::vec3 cameraToModelNormalized{ glm::normalize(
      gizmo->ModelMatrix[3].xyz() - g.Camera.Eye) };
    const glm::vec3 orthoDir{ glm::cross(dir, cameraToModelNormalized) };
    movePlaneNormal = glm::normalize(glm::cross(dir, orthoDir));
  } else {
    movePlaneNormal = -g.Camera.Forward;
  }

  return BuildPlane(gizmo->ModelMatrix[3], movePlaneNormal);
}
void BeginTranslation() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->DragTranslationOrigin = gizmo->ModelMatrix[3];
  gizmo->TranslationPlane = BuildTranslatePlane();
  const auto length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
  gizmo->ModelRelativeOrigin =
    (gizmo->TranslationPlaneOrigin - gizmo->ModelMatrix[3].xyz()) *
    (1.0f / gizmo->ScreenFactor);

  g.DragOrigin = io.MousePos;
}
void ContinueTranslation(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };

  const float length{ glm::abs(
    IntersectRayPlane(g.Ray, gizmo->TranslationPlane)) };
  const glm::vec3 targetPosition{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 newPosition{ targetPosition - gizmo->ModelRelativeOrigin *
                                                  gizmo->ScreenFactor };
  glm::vec3 delta{ newPosition - gizmo->ModelMatrix[3].xyz() };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axisIdx{ GetAxisIdx(flags) };
    const glm::vec3 axisValue{ gizmo->ModelMatrix[axisIdx] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
  }

  if (snap) {
    glm::vec3 cumulativeDelta{ gizmo->ModelMatrix[3].xyz() + delta -
                               gizmo->DragTranslationOrigin };
    bool applyRotationLocaly{ gizmo->Mode == ImGuizmoMode_Local ||
                              flags == ImGuizmoAxisFlags_ALL };
    if (applyRotationLocaly) {
      glm::mat4 modelSourceNormalized{ gizmo->SourceModelMatrix };
      for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
        modelSourceNormalized[axis] =
          glm::normalize(modelSourceNormalized[axis]);

      const glm::mat4 inversedModelSourceNormalized{ glm::inverse(
        modelSourceNormalized) };
      cumulativeDelta =
        inversedModelSourceNormalized * glm::vec4{ cumulativeDelta, 0.0f };
      ComputeSnap(cumulativeDelta, snap);
      cumulativeDelta =
        modelSourceNormalized * glm::vec4{ cumulativeDelta, 0.0f };
    } else {
      ComputeSnap(cumulativeDelta, snap);
    }
    delta = gizmo->DragTranslationOrigin + cumulativeDelta -
            gizmo->ModelMatrix[3].xyz();
  }

  const glm::mat4 translationMatrix{ glm::translate(delta) };
  if (delta != gizmo->LastTranslationDelta) {
    gizmo->ModelMatrix = translationMatrix * gizmo->SourceModelMatrix;
    gizmo->Dirty = true;
  }
  gizmo->LastTranslationDelta = delta;
}

void RenderAxis(ImGuizmoAxis axisIdx, ImGuizmoMoveDirection flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  //const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };

  ImU32 color{ GetColorU32(ImGuizmoCol_AxisX + axisIdx) };
  if (HasSingleAxis(flags) && GetAxisIdx(flags) == axisIdx)
    color = GetColorU32(ImGuizmoCol_Selection, 0.5f);

  const glm::vec3 &dirAxis{ kUnitDirections[axisIdx] };
  const glm::vec2 tail{ WorldToScreen(dirAxis * 0.1f * gizmo->ScreenFactor,
                                      gizmo->ModelViewProjMatrix) };
  const glm::vec2 head{ WorldToScreen(dirAxis * gizmo->ScreenFactor,
                                      gizmo->ModelViewProjMatrix) };

  g.DrawList->AddLine(tail, head, color, kLineThickness);

  // ---

  constexpr float kArrowheadSize{ kLineThickness * 2.0f };
  const glm::vec2 dir{ glm::normalize(gizmo->Origin - head) * kArrowheadSize };
  const glm::vec2 orthogonalDir{ dir.y, -dir.x };
  const glm::vec2 a{ head + dir };
  g.DrawList->AddTriangleFilled(head - dir, a + orthogonalDir,
                                a - orthogonalDir, color);
}
void RenderPlane(ImGuizmoPlane planeIdx, ImGuizmoMoveDirection flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_PlaneYZ + planeIdx) };
  if (HasPlane(flags) && GetPlaneIdx(flags) == planeIdx)
    color = GetColorU32(ImGuizmoCol_Selection, 0.5f);

  ImVec2 screenQuadPts[4]{};
  for (int i = 0; i < 4; ++i) {
    const glm::vec3 cornerWorldSpace{
      (kUnitDirections[(planeIdx + 1) % 3] * kUnitQuad[i * 2] +
       kUnitDirections[(planeIdx + 2) % 3] * kUnitQuad[i * 2 + 1]) *
      gizmo->ScreenFactor
    };
    screenQuadPts[i] =
      WorldToScreen(cornerWorldSpace, gizmo->ModelViewProjMatrix);
  }

  g.DrawList->AddConvexPolyFilled(screenQuadPts, 4, color);
  constexpr float kQuadBorder{ 1.5f };
  g.DrawList->AddPolyline(screenQuadPts, 4, color | 0x60000000, true,
                          kQuadBorder);
}

void RenderOmniDot(ImGuizmoMoveDirection flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_SpecialMove) };
  if (flags == ImGuizmoAxisFlags_ALL)
    color = GetColorU32(ImGuizmoCol_Selection, 0.5f);
  g.DrawList->AddCircleFilled(gizmo->Origin, kCircleRadius, color, 32);
}

void RenderTranslationTrail() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const glm::vec2 tail{ WorldToScreen(gizmo->DragTranslationOrigin,
                                      g.Camera.ViewProjectionMatrix) };
  const glm::vec2 head{ WorldToScreen(gizmo->ModelMatrix[3],
                                      g.Camera.ViewProjectionMatrix) };
  const glm::vec2 diff{ glm::normalize(head - tail) * (kCircleRadius - 1.0f) };

  constexpr auto kTranslationLineColor = 0xAAAAAAAA;
  constexpr float kMargin{ 1.5f };
  g.DrawList->AddCircle(tail, kCircleRadius + kMargin, kTranslationLineColor);
  g.DrawList->AddCircle(head, kCircleRadius + kMargin, kTranslationLineColor);
  g.DrawList->AddLine(tail + diff, head - diff, kTranslationLineColor, 2.0f);
}

//-----------------------------------------------------------------------------
// [SECTION] ROTATION
//-----------------------------------------------------------------------------

glm::vec4 BuildRotationPlane() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };

  glm::vec3 point, planeNormal;
  if (HasSingleAxis(flags)) {
    point = gizmo->Mode == ImGuizmoMode_Local ? gizmo->ModelMatrix[3]
                                              : gizmo->SourceModelMatrix[3];
    planeNormal = gizmo->ModelMatrix[GetAxisIdx(flags)];
  } else {
    point = gizmo->SourceModelMatrix[3];
    planeNormal = -g.Camera.Forward;
  }
  return BuildPlane(point, planeNormal);
}
void BeginRotation() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->TranslationPlane = BuildRotationPlane();
  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->RotationVectorSource = glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - gizmo->ModelMatrix[3].xyz());
  gizmo->RotationAngleOrigin = gizmo->ComputeAngleOnPlane();

  g.DragOrigin = io.MousePos;
}
void ContinueRotation(const float *snap) {
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->RotationAngle = gizmo->ComputeAngleOnPlane();
  if (snap) ComputeSnap(gizmo->RotationAngle, glm::radians(snap[0]));

  const glm::vec3 rotationAxisLocalSpace{ glm::normalize(
    glm::mat3{ gizmo->InversedModelMatrix } *
    glm::vec3{ gizmo->TranslationPlane }) };
  const glm::mat4 deltaRotation{ glm::rotate(gizmo->RotationAngle -
                                               gizmo->RotationAngleOrigin,
                                             rotationAxisLocalSpace) };

  if (gizmo->RotationAngle != gizmo->RotationAngleOrigin) {
    if (gizmo->Mode == ImGuizmoMode_Local) {
      const glm::mat4 scaleOrigin{ glm::scale(gizmo->ModelScaleOrigin) };
      gizmo->ModelMatrix *= deltaRotation * scaleOrigin;
    } else {
      glm::mat4 result{ gizmo->SourceModelMatrix };
      result[3] = glm::vec4{ glm::vec3{ 0.0f }, 1.0f };
      gizmo->ModelMatrix = deltaRotation * result;
      gizmo->ModelMatrix[3] = gizmo->SourceModelMatrix[3];
    }
    gizmo->Dirty = true;
  }
  gizmo->RotationAngleOrigin = gizmo->RotationAngle;
}

bool MouseOverRotationRing() {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoStyle &style{ GetStyle() };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  constexpr float kTolerance{ 1.0f };
  const float kRingThickness{ style.RotationRingThickness + kTolerance };
  const float distance{ glm::length(glm::vec2{ io.MousePos } - gizmo->Origin) };
  return (distance >= gizmo->RingRadius - kRingThickness) &&
         (distance < gizmo->RingRadius + kRingThickness);
}
bool MouseOverRotationAxis(ImGuizmoAxis axisIdx) {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const glm::vec4 pickupPlane{ BuildPlane(gizmo->ModelMatrix[3],
                                          gizmo->ModelMatrix[axisIdx]) };
  const float length{ IntersectRayPlane(g.Ray, pickupPlane) };
  const glm::vec3 localPos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - gizmo->ModelMatrix[3].xyz()) };

  // ... ?
  if (glm::dot(localPos, g.Ray.Direction) > kEpsilon) return false;

  const glm::vec3 idealPosOnCircle{ glm::mat3{ gizmo->InversedModelMatrix } *
                                    localPos };
  const glm::vec2 idealPosOnCircleScreen{ WorldToScreen(
    idealPosOnCircle * gizmo->ScreenFactor, gizmo->ModelViewProjMatrix) };

  constexpr float kTolerance{ 8.0f };
  const glm::vec2 distanceOnScreen{ idealPosOnCircleScreen - io.MousePos };
  return glm::length(distanceOnScreen) < kTolerance;
}

void RenderRotationAxis(ImGuizmoAxis axisIdx, ImGuizmoMoveDirection flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 col{ GetColorU32(ImGuizmoCol_AxisX + axisIdx) };
  if (HasSingleAxis(flags) && GetAxisIdx(flags) == axisIdx)
    col = GetColorU32(ImGuizmoCol_Selection, 0.5f);

  glm::vec3 cameraToModelNormalized =
    g.Camera.IsOrtho
      ? -glm::inverse(g.Camera.ViewMatrix)[2]
      : glm::normalize(gizmo->ModelMatrix[3].xyz() - g.Camera.Eye);
  cameraToModelNormalized =
    gizmo->InversedModelMatrix * glm::vec4{ cameraToModelNormalized, 0.0f };

  const float angleStart{ (glm::atan(
                            cameraToModelNormalized[(4 - axisIdx) % 3],
                            cameraToModelNormalized[(3 - axisIdx) % 3])) +
                          kPi * 0.5f };

  ImVec2 circlePos[kArcSegmentCount]{};
  for (int i = 0; i < kArcSegmentCount; ++i) {
    const float ng{ angleStart +
                    kPi * (static_cast<float>(i) / kArcSegmentCount) };
    const glm::vec3 axisPos{ glm::cos(ng), glm::sin(ng), 0.0f };
    const auto pos = glm::vec3{ axisPos[axisIdx], axisPos[(axisIdx + 1) % 3],
                                axisPos[(axisIdx + 2) % 3] } *
                     gizmo->ScreenFactor;
    circlePos[i] = WorldToScreen(pos, gizmo->ModelViewProjMatrix);
  }

  const float radiusAxis{ glm::length(gizmo->Origin -
                                      glm::vec2{ circlePos[0] }) };

  // @todo Ring radius should be computed elsewhere, and certainly not in rendering
  if (radiusAxis > gizmo->RingRadius) gizmo->RingRadius = radiusAxis;
  g.DrawList->AddPolyline(circlePos, kArcSegmentCount, col, false, 2.5f);
}
void RenderRotationRing(ImGuizmoMoveDirection flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 col = GetColorU32(ImGuizmoCol_SpecialMove);
  if (flags == ImGuizmoAxisFlags_ALL)
    col = GetColorU32(ImGuizmoCol_Selection, 0.5f);

  g.DrawList->AddCircle(gizmo->Origin, gizmo->RingRadius, col, 64,
                        g.Style.RotationRingThickness);
}

void RenderRotationTrail() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 col = GetColorU32(ImGuizmoCol_Selection, 0.541f);
  ImU32 border = GetColorU32(ImGuizmoCol_Selection);

  ImVec2 circlePoints[kArcSegmentCount + 1]{ gizmo->Origin };
  for (int i = 1; i < kArcSegmentCount; ++i) {
    const float ng{ gizmo->RotationAngle *
                    (static_cast<float>(i - 1) / (kArcSegmentCount - 1)) };
    const glm::mat3 rotateVectorMatrix{ glm::rotate(
      ng, glm::vec3{ gizmo->TranslationPlane }) };
    glm::vec3 pos{ rotateVectorMatrix * gizmo->RotationVectorSource };
    pos *= gizmo->ScreenFactor;
    circlePoints[i] = WorldToScreen(pos + gizmo->ModelMatrix[3].xyz(),
                                    g.Camera.ViewProjectionMatrix);
  }

  g.DrawList->AddConvexPolyFilled(circlePoints, kArcSegmentCount, col);
  g.DrawList->AddPolyline(circlePoints, kArcSegmentCount, border, true, 2.0f);
}

//-----------------------------------------------------------------------------
// [SECTION] SCALE
//-----------------------------------------------------------------------------

glm::vec4 BuildScalePlane() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axisIdx{ GetAxisIdx(flags) };
    return BuildPlane(gizmo->ModelMatrix[3],
                      gizmo->ModelMatrix[GetScaleAxisIdx(axisIdx)]);
  } else {
    return BuildPlane(gizmo->ModelMatrix[3], gizmo->ModelMatrix[2]);
  }
}

void BeginScale() {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->Scale = glm::vec3{ 1.0f };
  gizmo->DragTranslationOrigin = gizmo->ModelMatrix[3];
  gizmo->TranslationPlane = BuildScalePlane();
  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
  gizmo->ModelRelativeOrigin =
    (gizmo->TranslationPlaneOrigin - gizmo->ModelMatrix[3].xyz()) *
    (1.0f / gizmo->ScreenFactor);

  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
    gizmo->ScaleValueOrigin[axis] = glm::length(gizmo->SourceModelMatrix[axis]);

  g.DragOrigin = ImGui::GetIO().MousePos;
}
void ContinueScale(const float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  const glm::vec3 targetPosition{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 newPosition{ targetPosition - gizmo->ModelRelativeOrigin *
                                                  gizmo->ScreenFactor };
  glm::vec3 delta{ newPosition - gizmo->ModelMatrix[3].xyz() };

  const ImGuizmoMoveDirection flags{ gizmo->ActiveManipulation };
  if (HasSingleAxis(flags)) {
    const int axisIndex{ GetAxisIdx(flags) };
    const glm::vec3 axisValue{ gizmo->ModelMatrix[axisIndex] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
    const glm::vec3 baseVector{ gizmo->TranslationPlaneOrigin -
                                gizmo->ModelMatrix[3].xyz() };
    const float ratio{ glm::dot(axisValue, baseVector + delta) /
                       glm::dot(axisValue, baseVector) };
    gizmo->Scale[axisIndex] = glm::max(ratio, 0.001f);
  } else {
    const float scaleDelta{ (io.MousePos.x - g.DragOrigin.x) * 0.01f };
    gizmo->Scale = glm::vec3{ glm::max(1.0f + scaleDelta, 0.001f) };
  }

  if (snap) {
    const float scaleSnap[]{ snap[0], snap[0], snap[0] };
    ComputeSnap(gizmo->Scale, scaleSnap);
  }

  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis)
    gizmo->Scale[axis] = glm::max(gizmo->Scale[axis], 0.001f);

  bool modified{ gizmo->LastScale != gizmo->Scale };
  if (modified) {
    const glm::mat4 deltaMatrixScale{ glm::scale(gizmo->Scale *
                                                 gizmo->ScaleValueOrigin) };
    gizmo->ModelMatrix *= deltaMatrixScale;
    gizmo->Dirty = true;
  }
  gizmo->LastScale = gizmo->Scale;
}

//-----------------------------------------------------------------------------
// [SECTION] CAGE
//-----------------------------------------------------------------------------

//
// @todo ...
//

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

/** @todo Remove me */
void ComputeFrustumPlanes(glm::vec4 frustum[6], const float *clip) {
  frustum[0].x = clip[3] - clip[0];
  frustum[0].y = clip[7] - clip[4];
  frustum[0].z = clip[11] - clip[8];
  frustum[0].w = clip[15] - clip[12];

  frustum[1].x = clip[3] + clip[0];
  frustum[1].y = clip[7] + clip[4];
  frustum[1].z = clip[11] + clip[8];
  frustum[1].w = clip[15] + clip[12];

  frustum[2].x = clip[3] + clip[1];
  frustum[2].y = clip[7] + clip[5];
  frustum[2].z = clip[11] + clip[9];
  frustum[2].w = clip[15] + clip[13];

  frustum[3].x = clip[3] - clip[1];
  frustum[3].y = clip[7] - clip[5];
  frustum[3].z = clip[11] - clip[9];
  frustum[3].w = clip[15] - clip[13];

  frustum[4].x = clip[3] - clip[2];
  frustum[4].y = clip[7] - clip[6];
  frustum[4].z = clip[11] - clip[10];
  frustum[4].w = clip[15] - clip[14];

  frustum[5].x = clip[3] + clip[2];
  frustum[5].y = clip[7] + clip[6];
  frustum[5].z = clip[11] + clip[10];
  frustum[5].w = clip[15] + clip[14];

  for (int i = 0; i < 6; ++i)
    frustum[i] = glm::normalize(frustum[i]);
}

//-----------------------------------------------------------------------------
// [SECTION] PUBLIC INTERFACE
//-----------------------------------------------------------------------------

void PrintContext() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ g.ActiveGizmo };

  ImGui::Begin("ImGuizmo::Debug");

  auto topLeft = g.Viewport.GetTL();
  auto size = g.Viewport.GetSize();
  ImGui::Text("Viewport = (%.f,%.f) %.fx%.f", topLeft.x, topLeft.y, size.x,
              size.y);

  if (ImGui::TreeNode("Camera")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("Right", &g.Camera.Right[0], "%.2f");
    ImGui::InputFloat3("Up", &g.Camera.Up[0], "%.2f");
    ImGui::InputFloat3("Forward", &g.Camera.Forward[0], "%.2f");
    ImGui::InputFloat3("Eye", &g.Camera.Eye[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Ray")) {
    ImGui::Text("x: %.f y: %.f", io.MousePos.x, io.MousePos.y);

    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
    ImGui::InputFloat3("Start", &g.Ray.Start[0], "%.2f");
    ImGui::InputFloat3("End", &g.Ray.End[0], "%.2f");
    ImGui::InputFloat3("Direction", &g.Ray.Direction[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (gizmo) {
    ImGui::Text("ID = %d", gizmo->ID);
    ImGui::Text("ActiveOperation: %d", gizmo->ActiveOperation);
    ImGui::Text("ActiveManipulation: %d", gizmo->ActiveManipulation);

    if (ImGui::TreeNode("Gizmo")) {
      ImGui::Text("Origin (screen space): [%.2f, %.2f]", gizmo->Origin.x,
                  gizmo->Origin.y);
      ImGui::Text("RingRadius: %.2f", gizmo->RingRadius);
      ImGui::TreePop();
    }


#if 0
  if (ImGui::TreeNode("Bounds")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Checkbox("InUse", &g.Bounds.InUse);
    ImGui::InputFloat3("Pivot", &g.Bounds.Pivot[0]);
    ImGui::InputFloat3("Anchor", &g.Bounds.Anchor[0]);
    ImGui::InputFloat3("LocalPivot", &g.Bounds.LocalPivot[0]);
    ImGui::InputFloat4("Plane", &g.Bounds.Plane[0]);
    ImGui::InputInt("BestAxis", &g.Bounds.BestAxis);
    ImGui::InputInt2("Axis", &g.Bounds.Axis[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
#endif

  if (ImGui::TreeNode("Translation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("DragTranslationOrigin",
                       &gizmo->DragTranslationOrigin[0]);
    ImGui::InputFloat3("LastTranslationDelta", &gizmo->LastTranslationDelta[0]);
    ImGui::InputFloat3("ModelRelativeOrigin", &gizmo->ModelRelativeOrigin[0]);
    ImGui::InputFloat3("TranslationPlaneOrigin",
                       &gizmo->TranslationPlaneOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Rotation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("RotationVectorSource", &gizmo->RotationVectorSource[0]);
    ImGui::InputFloat("RotationAngle", &gizmo->RotationAngle);
    ImGui::InputFloat("RotationAngleOrigin", &gizmo->RotationAngleOrigin);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Scale")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("ModelScaleOrigin", &gizmo->ModelScaleOrigin[0]);
    ImGui::InputFloat3("ScaleValueOrigin", &gizmo->ScaleValueOrigin[0]);
    ImGui::InputFloat3("Scale", &gizmo->Scale[0]);
    ImGui::InputFloat3("LastScale", &gizmo->LastScale[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  }

  ImGui::End();
}

void SetViewport(const ImVec2 &position, const ImVec2 &size) {
  SetViewport({ position, position + size });
}
void SetViewport(float x, float y, float width, float height) {
  SetViewport({ x, y }, { width, height });
}
void SetDrawlist(ImDrawList *drawList) {
  GImGuizmo.DrawList = drawList ? drawList : ImGui::GetWindowDrawList();
}

void CreateCanvas(const char *name) {
  ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
  ImGui::PushStyleColor(ImGuiCol_Border, 0);
  ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

  constexpr ImGuiWindowFlags flags{
    ImGuiWindowFlags_NoBackground | ImGuiWindowFlags_NoDecoration |
    ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoInputs |
    ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoBringToFrontOnFocus
  };

  ImGui::Begin(name, nullptr, flags);
  SetDrawlist(ImGui::GetWindowDrawList());
  SetViewport(ImGui::GetWindowPos(), ImGui::GetWindowSize());
  ImGui::End();
  ImGui::PopStyleVar();
  ImGui::PopStyleColor(2);
}
void CreateCanvas(const char *name, const ImVec2 &position,
                  const ImVec2 &size) {
  ImGui::SetNextWindowPos(position);
  ImGui::SetNextWindowSize(size);
  CreateCanvas(name);
}

void Manipulate(ImGuizmoMode mode, ImGuizmoOperation operation, float *model,
                const float *snap) {
  Begin(mode, model);
  switch (operation) {
  case ImGuizmoOperation_Translate:
    Translate(snap);
    break;
  case ImGuizmoOperation_Rotate:
    Rotate(snap);
    break;
  case ImGuizmoOperation_Scale:
    Scale(snap);
    break;
  }
  End();
}

void SetCamera(const float *view, const float *projection, bool isOrtho) {
  IM_ASSERT(view && projection);

  ImGuizmoContext &g{ GImGuizmo };

  g.Camera.ViewMatrix = glm::make_mat4(view);
  const glm::mat4 inversedViewMatrix{ glm::inverse(g.Camera.ViewMatrix) };
  g.Camera.Right = inversedViewMatrix[0];
  g.Camera.Up = inversedViewMatrix[1];
  g.Camera.Forward = inversedViewMatrix[2];
  g.Camera.Eye = inversedViewMatrix[3];

  g.Camera.IsOrtho = isOrtho;
  g.Camera.ProjectionMatrix = glm::make_mat4(projection);

  g.Camera.ViewProjectionMatrix =
    g.Camera.ProjectionMatrix * g.Camera.ViewMatrix;
}

bool Begin(ImGuizmoMode mode, float *model) {
  IM_ASSERT(model && "Model matrix required");

  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  IM_ASSERT(g.DrawList != nullptr);
  IM_ASSERT(g.LockedModelMatrix == nullptr && "Nesting forbidden");
  g.LockedModelMatrix = model;

  auto id = static_cast<ImGuiID>(reinterpret_cast<long long>(model));
  ImGuizmoWidget *gizmo{ FindGizmoById(id) };
  if (gizmo == nullptr) gizmo = CreateNewGizmo(id);
  g.CurrentGizmo = gizmo;

  if (!io.MouseDown[0]) {
    g.ActiveGizmo = nullptr;
    gizmo->ActiveManipulation = ImGuizmoAxisFlags_None;
  }
  if (gizmo->ActiveManipulation == ImGuizmoAxisFlags_None)
    gizmo->ActiveOperation = ImGuizmoOperation_None;

  gizmo->Mode = mode;
  FeedGizmo(model);

  g.Ray = RayCast(g.Camera.ViewProjectionMatrix);

  const glm::vec3 cameraSpacePosition{ gizmo->ModelViewProjMatrix *
                                       glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  bool visible{ true };
  if (!g.Camera.IsOrtho && cameraSpacePosition.z < 0.001f) visible = false;
  return visible;
}
void End() {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };
  IM_ASSERT(g.LockedModelMatrix && "It seems that you didn't call Begin()");

  if (gizmo->Dirty) {
    *reinterpret_cast<glm::mat4 *>(g.LockedModelMatrix) = gizmo->ModelMatrix;
    gizmo->Dirty = false;
  }
  g.LockedModelMatrix = nullptr;

  g.CurrentGizmo = nullptr;
}

void Translate(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoMoveDirection hoverFlags{ ImGuizmoAxisFlags_None };
  if (MouseOverOrigin()) hoverFlags |= ImGuizmoAxisFlags_ALL;
  if (hoverFlags != ImGuizmoAxisFlags_ALL) {
    for (ImGuizmoPlane plane = 0; plane < 3; ++plane)
      if (MouseOverPlane(plane)) {
        hoverFlags |= PlaneToFlags(plane);
        break;
      }
    if (!HasPlane(hoverFlags)) {
      for (ImGuizmoAxis axis = 0; axis < 3; ++axis)
        if (MouseOverAxis(axis)) {
          hoverFlags |= AxisToFlag(axis);
          break;
        }
    }
  }

  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Translate, hoverFlags, &held))
    BeginTranslation();
  if (held) ContinueTranslation(snap);

  //
  // Render:
  //

  if (gizmo->ActiveOperation == ImGuizmoOperation_Translate)
    RenderTranslationTrail();

  for (ImGuizmoAxis axis = 0; axis < 3; ++axis)
    RenderAxis(axis, hoverFlags);
  for (ImGuizmoPlane plane = 0; plane < 3; ++plane)
    RenderPlane(plane, hoverFlags);
  RenderOmniDot(hoverFlags);

  if (gizmo->ActiveOperation == ImGuizmoOperation_Translate)
    RenderTranslationInfo();
}
void Rotate(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoMoveDirection hoverFlags{ ImGuizmoAxisFlags_None };
  if (MouseOverRotationRing()) hoverFlags |= ImGuizmoAxisFlags_ALL;
  if (hoverFlags != ImGuizmoAxisFlags_ALL) {
    for (ImGuizmoAxis axis = 0; axis < 3; ++axis) {
      int aroundAxis{ GetAxisAroundIdx(axis) };
      if (MouseOverRotationAxis(aroundAxis)) {
        hoverFlags |= AxisToFlag(aroundAxis);
        break;
      }
    }
  }

  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Rotate, hoverFlags, &held))
    BeginRotation();
  if (held) ContinueRotation(snap);

  //
  // Render:
  //

  for (ImGuizmoAxis axis = 0; axis < 3; ++axis)
    RenderRotationAxis(GetAxisAroundIdx(axis), hoverFlags);
  
  RenderRotationRing(hoverFlags);

  if (gizmo->ActiveOperation == ImGuizmoOperation_Rotate) {
    RenderRotationTrail();
    RenderRotationInfo();
  }
}
void Scale(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoMoveDirection hoverFlags{ ImGuizmoAxisFlags_None };
  if (MouseOverOrigin()) hoverFlags |= ImGuizmoAxisFlags_ALL;
  if (hoverFlags != ImGuizmoAxisFlags_ALL) {
    for (ImGuizmoAxis axis = 0; axis < 3; ++axis)
      if (MouseOverAxis(axis)) {
        hoverFlags |= AxisToFlag(axis);
        break;
      }
  }

  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Scale, hoverFlags, &held)) {
    BeginScale();
  }
  if (held) ContinueScale(snap);

  //
  // Render:
  //

  for (ImGuizmoAxis axis = 0; axis < 3; ++axis) {
    RenderAxis(axis, hoverFlags);
  }
  RenderOmniDot(hoverFlags);

  if (gizmo->ActiveOperation == ImGuizmoOperation_Scale) RenderScaleInfo();
}

void Cage(const float *bounds, const float *snap) {}

bool ViewManipBehavior(ImGuiID id, bool inside, bool *out_held) {
  const ImGuiIO &io{ ImGui::GetIO() };

  bool pressed{ false };
  bool held{ false };
  if (inside) {
    if (io.MouseClicked[0]) {
      pressed = true;
    }
  }

  if (io.MouseDown[0] &&
      glm::any(glm::greaterThan(glm::abs(glm::vec2{ io.MouseDelta }),
                                glm::vec2{ 0.0f }))) {
    held = true;
  }

    /*
  if (!pressed) {
    if (io.MouseDown[0] && ImGui::IsWindowFocused() &&
        (io.MouseDelta.x || io.MouseDelta.y)) {
      held = true;
    }
  }*/

  if (out_held) *out_held = held;

  return pressed;
}

void ViewManip(float *view, const float length, const ImVec2 &position,
               const ImVec2 &size, ImU32 backgroundColor) {
  const ImGuiIO &io{ ImGui::GetIO() };

  //ImGui::GetItemID();
  ImGuiID parentId{ ImGui::GetCurrentWindow()->ID };
  //ImGui::GetCurrentContext()->ActiveIdWindow

  auto win = ImGui::GetCurrentWindow();
  win->GetID(0);

  ImGui::Text("win id = %d", win->ID);
  ImGui::Text("itm id = %d", ImGui::GetItemID());
  ImGui::Text("act id = %d", ImGui::IsItemActive());
  //  ImGui::Text("isactiv = %d", );

  

  bool isInside{ ImRect(position, position + size).Contains(io.MousePos) };
  
  if (isInside) {
    ImGui::Text("yeah!");
  }

  bool held;
  if (ViewManipBehavior(parentId, isInside, &held)) {
    ImGui::Text("click!");
  }

  if (held) {
    ImGui::Text("drag!");
  }

}

void ViewManipulate(float *view, const float length, const ImVec2 &position,
                    const ImVec2 &size, ImU32 backgroundColor) {
  const ImGuiIO &io{ ImGui::GetIO() };

  ImDrawList *drawList{ ImGui::GetCurrentWindow()->DrawList };
  drawList->AddRectFilled(position, position + size, backgroundColor);

  static bool isDragging{ false };
  static bool isClicking{ false };
  static bool isInside{ false };

  static glm::vec3 interpolationUp{ 0.0f };
  static glm::vec3 interpolationDir{ 0.0f };
  static int interpolationFrames{ 0 };

  constexpr float kDistance{ 2.0f };
  const glm::mat4 cubeProjection{ glm::perspective(
    glm::radians(60.0f), size.x / size.y, 0.01f, 1000.0f) };

  const glm::mat4 inversedViewMatrix{ glm::inverse(
    *reinterpret_cast<const glm::mat4 *>(view)) };
  const glm::vec3 forward{ inversedViewMatrix[2][0], inversedViewMatrix[2][1],
                           inversedViewMatrix[2][2] };
  const glm::vec3 up{ inversedViewMatrix[1][0], inversedViewMatrix[1][1],
                      inversedViewMatrix[1][2] };
  const glm::vec3 eye{ forward * kDistance };
  const glm::mat4 cubeView{ glm::lookAt(eye, glm::vec3{ 0.0f }, up) };

  const ImGuizmoRay ray{ RayCast(cubeProjection * cubeView, position, size) };

  static const glm::vec2 kPanelPosition[]{ { 0.75f, 0.75f }, { 0.25f, 0.75f },
                                           { 0.00f, 0.75f }, { 0.75f, 0.25f },
                                           { 0.25f, 0.25f }, { 0.00f, 0.25f },
                                           { 0.75f, 0.00f }, { 0.25f, 0.00f },
                                           { 0.00f, 0.00f } };
  static const glm::vec2 kPanelSize[]{ { 0.25f, 0.25f }, { 0.50f, 0.25f },
                                       { 0.25f, 0.25f }, { 0.25f, 0.50f },
                                       { 0.50f, 0.50f }, { 0.25f, 0.50f },
                                       { 0.25f, 0.25f }, { 0.50f, 0.25f },
                                       { 0.25f, 0.25f } };

  bool boxes[27]{};
  for (int iPass = 0; iPass < 2; ++iPass) {
    for (int iFace = 0; iFace < 6; ++iFace) {
      const int normalIndex{ iFace % 3 };
      const int perpXIndex{ (normalIndex + 1) % 3 };
      const int perpYIndex{ (normalIndex + 2) % 3 };

      const float invert{ (iFace > 2) ? -1.0f : 1.0f };
      const glm::vec3 indexVectorX{ kUnitDirections[perpXIndex] * invert };
      const glm::vec3 indexVectorY{ kUnitDirections[perpYIndex] * invert };
      const glm::vec3 boxOrigin{ kUnitDirections[normalIndex] * -invert -
                                 indexVectorX - indexVectorY };

      // Plane local space
      const glm::vec3 n{ kUnitDirections[normalIndex] * invert };
      const glm::vec3 viewSpaceNormal{ glm::vec3{
        glm::normalize(cubeView * glm::vec4{ n, 0.0f }) } };
      const glm::vec3 viewSpacePoint{ cubeView * glm::vec4{ n * 0.5f, 1.0f } };
      const glm::vec4 viewSpaceFacePlane{ BuildPlane(viewSpacePoint,
                                                     viewSpaceNormal) };

      if (viewSpaceFacePlane.w > 0) continue; // Back face culling

      const glm::vec4 facePlane{ BuildPlane(n * 0.5f, n) };
      const float len{ IntersectRayPlane(ray, facePlane) };
      const glm::vec3 posOnPlane{ ray.Start + ray.Direction * len -
                                  (n * 0.5f) };

      const float localX{
        glm::dot(kUnitDirections[perpXIndex], posOnPlane) * invert + 0.5f
      };
      const float localY{
        glm::dot(kUnitDirections[perpYIndex], posOnPlane) * invert + 0.5f
      };

      // Panels
      const glm::vec3 dx{ kUnitDirections[perpXIndex] };
      const glm::vec3 dy{ kUnitDirections[perpYIndex] };
      const glm::vec3 origin{ kUnitDirections[normalIndex] - dx - dy };
      for (int iPanel = 0; iPanel < 9; ++iPanel) {
        const glm::vec2 p{ kPanelPosition[iPanel] * 2.0f };
        const glm::vec2 s{ kPanelSize[iPanel] * 2.0f };
        const glm::vec3 panelPos[4]{ dx * p.x + dy * p.y,
                                     dx * p.x + dy * (p.y + s.y),
                                     dx * (p.x + s.x) + dy * (p.y + s.y),
                                     dx * (p.x + s.x) + dy * p.y };

        ImVec2 faceCoordsScreen[4];
        for (auto iCoord = 0; iCoord < 4; ++iCoord) {
          faceCoordsScreen[iCoord] =
            WorldToScreen((panelPos[iCoord] + origin) * 0.5f * invert,
                          cubeProjection * cubeView, position, size);
        }

        const glm::vec2 panelCorners[2]{
          kPanelPosition[iPanel], kPanelPosition[iPanel] + kPanelSize[iPanel]
        };
        const bool insidePanel{ localX > panelCorners[0].x &&
                                localX < panelCorners[1].x &&
                                localY > panelCorners[0].y &&
                                localY < panelCorners[1].y };

        const glm::vec3 boxCoord{
          boxOrigin + indexVectorX * static_cast<float>(iPanel % 3) +
          indexVectorY * static_cast<float>(iPanel / 3) + glm::vec3{ 1.0f }
        };
        const auto boxCoordInt{ static_cast<int>(
          boxCoord.x * 9.0f + boxCoord.y * 3.0f + boxCoord.z) };
        IM_ASSERT(boxCoordInt < 27);

        boxes[boxCoordInt] |= insidePanel && (!isDragging);

        // Draw face with lighter color
        if (iPass) {
          drawList->AddConvexPolyFilled(
            faceCoordsScreen, 4,
            (GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0xff1f1f1f) |
              (isInside ? 0x080808 : 0));
          if (boxes[boxCoordInt]) {
            drawList->AddConvexPolyFilled(faceCoordsScreen, 4, 0x8060A0F0);

            if (!io.MouseDown[0] && !isDragging && isClicking) {
              // Apply new view direction
              const int cx{ boxCoordInt / 9 };
              const int cy{ (boxCoordInt - cx * 9) / 3 };
              const int cz{ boxCoordInt % 3 };
              interpolationDir = glm::normalize(1.0f - glm::vec3{ cx, cy, cz });

              if (glm::abs(glm::dot(interpolationDir, kReferenceUp)) >
                  1.0f - 0.01f) {
                glm::vec3 right{ inversedViewMatrix[0] };
                if (glm::abs(right.x) > glm::abs(right.z)) {
                  right.z = 0.0f;
                } else {
                  right.x = 0.0f;
                }
                right = glm::normalize(right);
                interpolationUp =
                  glm::normalize(glm::cross(interpolationDir, right));
              } else {
                interpolationUp = kReferenceUp;
              }
              interpolationFrames = 40;
              isClicking = false;
            }
            if (io.MouseDown[0] && !isDragging) {
              isClicking = true;
            }
          }
        }
      }
    }
  }

  //
  //
  //

  ImGui::Text("infr = %d", interpolationFrames);

  const glm::vec3 cameraTarget{ inversedViewMatrix[3] -
                                inversedViewMatrix[2] * length };

  if (interpolationFrames) {
    interpolationFrames--;

    const glm::vec3 newDir{ glm::normalize(
      glm::lerp(glm::vec3{ inversedViewMatrix[2] }, interpolationDir, 0.2f)) };
#if 0
    const glm::vec3 newUp{ glm::normalize(
      glm::lerp(glm::vec3{ inversedViewMatrix[1] }, interpolationUp, 0.3f)) };
#else
    const glm::vec3 newUp{ interpolationUp };
#endif
    const glm::vec3 newEye{ cameraTarget + newDir * length };
    *reinterpret_cast<glm::mat4 *>(view) =
      glm::lookAt(newEye, cameraTarget, newUp);
  }
  isInside = ImRect(position, position + size).Contains(io.MousePos);

  //
  // Drag view
  //

  if (glm::any(glm::greaterThan(glm::abs(glm::vec2{ io.MouseDelta }),
                                glm::vec2{ 0.0f }))) {
  }

  if (!isDragging && io.MouseDown[0] && isInside &&
      (glm::abs(io.MouseDelta.x) > 0.0f || glm::abs(io.MouseDelta.y) > 0.0f)) {
    isDragging = true;
    isClicking = false;
  } else if (isDragging && !io.MouseDown[0]) {
    isDragging = false;
  }

  if (isDragging) {
    auto angles = -glm::vec2{ io.MouseDelta } * 0.01f;
    const glm::mat4 rx{ glm::rotate(angles.x, kReferenceUp) };
    const glm::mat4 ry{ glm::rotate(angles.y,
                                    glm::vec3{ inversedViewMatrix[0] }) };
    const glm::mat4 roll{ ry * rx };

    glm::vec3 newDir{ glm::normalize(roll * inversedViewMatrix[2]) };
    glm::vec3 planeDir{ glm::cross(glm::vec3{ inversedViewMatrix[0] },
                                   kReferenceUp) };

    planeDir.y = 0.0f;
    planeDir = glm::normalize(planeDir);
    const float dt{ glm::dot(planeDir, newDir) };
    ImGui::Text("dt = %.2f", dt);
    /*
    if (dt < 0.1f) {
      newDir += planeDir * dt;
      newDir = glm::normalize(newDir);
    }
    */

    if (dt > 0.1f) {

      const glm::vec3 newEye{ cameraTarget + newDir * length };
      *reinterpret_cast<glm::mat4 *>(view) =
        glm::lookAt(newEye, cameraTarget, kReferenceUp);
    }
  }

    ImGui::Text("isInside %d", isInside);
  ImGui::Text("isClicking %d", isClicking);
  ImGui::Text("isDragging %d", isDragging);
}

void DrawCubes(const float *view, const float *projection,
               const float *modelMatrices, int modelMatrixCount) {
  const ImGuizmoContext &g{ GImGuizmo };

  struct cubeFace_t {
    float z;
    ImVec2 faceCoordsScreen[4];
    ImU32 color;
  };
  ImVector<cubeFace_t> faces;
  for (int i = 0; i < modelMatrixCount * 6; ++i)
    faces.push_back(cubeFace_t{});

  glm::vec4 frustum[6]{};

  const glm::mat4 viewProjectionMatrix{ glm::make_mat4(projection) *
                                        glm::make_mat4(view) };
  ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));
  int cubeFaceCount = 0;
  for (int cube = 0; cube < modelMatrixCount; ++cube) {
    const glm::mat4 modelMatrix{ glm::make_mat4(&modelMatrices[cube * 16]) };
    const glm::mat4 modelViewProjMatrix{ viewProjectionMatrix * modelMatrix };

    for (int iFace = 0; iFace < 6; ++iFace) {
      const int normalIndex{ (iFace % 3) };
      const int perpXIndex{ (normalIndex + 1) % 3 };
      const int perpYIndex{ (normalIndex + 2) % 3 };
      const float invert{ (iFace > 2) ? -1.0f : 1.0f };

      const glm::vec3 faceCoords[4]{
        kUnitDirections[normalIndex] + kUnitDirections[perpXIndex] +
          kUnitDirections[perpYIndex],
        kUnitDirections[normalIndex] + kUnitDirections[perpXIndex] -
          kUnitDirections[perpYIndex],
        kUnitDirections[normalIndex] - kUnitDirections[perpXIndex] -
          kUnitDirections[perpYIndex],
        kUnitDirections[normalIndex] - kUnitDirections[perpXIndex] +
          kUnitDirections[perpYIndex],
      };

      const glm::vec3 centerPosition{
        modelMatrix *
        glm::vec4{ (kUnitDirections[normalIndex] * 0.5f * invert), 1.0f }
      };
      const glm::vec4 centerPositionVP{
        modelViewProjMatrix *
        glm::vec4{ (kUnitDirections[normalIndex] * 0.5f * invert), 1.0f }
      };

      bool inFrustum{ true };
      for (int iFrustum = 0; iFrustum < 6; ++iFrustum) {
        const float dist{ DistanceToPlane(centerPosition, frustum[iFrustum]) };
        if (dist < 0.0f) {
          inFrustum = false;
          break;
        }
      }
      if (!inFrustum) continue;

      cubeFace_t &cubeFace{ faces[cubeFaceCount] };
      for (int iCoord = 0; iCoord < 4; iCoord++) {
        cubeFace.faceCoordsScreen[iCoord] = WorldToScreen(
          faceCoords[iCoord] * 0.5f * invert, modelViewProjMatrix);
      }

      cubeFace.color = GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0x808080;
      // cubeFace.color = priv::GetColorU32(ImGuizmoCol_AxisX + normalIndex,
      // 0.9);//      | 0x808080;

      cubeFace.z = centerPositionVP.z / centerPositionVP.w;
      cubeFaceCount++;
    }
  }

  qsort(faces.begin(), cubeFaceCount, sizeof(cubeFace_t),
        [](void const *_a, void const *_b) {
          auto *a = (cubeFace_t *)_a;
          auto *b = (cubeFace_t *)_b;
          if (a->z < b->z) {
            return 1;
          }
          return -1;
        });

  for (int iFace = 0; iFace < cubeFaceCount; ++iFace) {
    const cubeFace_t &cubeFace{ faces[iFace] };
    g.DrawList->AddConvexPolyFilled(cubeFace.faceCoordsScreen, 4,
                                    cubeFace.color);
  }
}
void DrawGrid(const float *view, const float *projection, const float *model,
              const float gridSize) {
  ImGuizmoContext &g{ GImGuizmo };

  const glm::mat4 viewProjectionMatrix{ glm::make_mat4(projection) *
                                        glm::make_mat4(view) };

  glm::vec4 frustum[6]{};
  ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));

  const glm::mat4 modelViewProjMatrix{ viewProjectionMatrix *
                                       glm::make_mat4(model) };

  for (float f = -gridSize; f <= gridSize; f += 1.0f) {
    for (int dir = 0; dir < 2; dir++) {
      glm::vec3 ptA{ glm::vec3{ dir ? -gridSize : f, 0.0f,
                                dir ? f : -gridSize } };
      glm::vec3 ptB{ dir ? gridSize : f, 0.0f, dir ? f : gridSize };
      bool visible{ true };
      for (int i = 0; i < 6; i++) {
        float dA{ DistanceToPlane(ptA, frustum[i]) };
        float dB{ DistanceToPlane(ptB, frustum[i]) };
        if (dA < 0.0f && dB < 0.0f) {
          visible = false;
          break;
        }
        if (dA > 0.0f && dB > 0.0f) {
          continue;
        }
        if (dA < 0.0f) {
          float len = glm::abs(dA - dB);
          float t = glm::abs(dA) / len;
          ptA = glm::lerp(ptA, ptB, t);
        }
        if (dB < 0.0f) {
          float len = glm::abs(dB - dA);
          float t = glm::abs(dB) / len;
          ptB = glm::lerp(ptB, ptA, t);
        }
      }
      if (visible) {
        ImU32 col{ kMediumGrayColor };
        // auto colr = ImGui::ColorConvertU32ToFloat4(0xFF909090);

        col = (fmodf(fabsf(f), 10.0f) < kEpsilon) ? 0xFF909090 : col;
        col = (fabsf(f) < kEpsilon) ? kDarkGrayColor : col;

        float thickness{ 1.0f };
        thickness = (fmodf(fabsf(f), 10.0f) < kEpsilon) ? 1.5f : thickness;
        thickness = (fabsf(f) < kEpsilon) ? 2.3f : thickness;

        g.DrawList->AddLine(WorldToScreen(ptA, modelViewProjMatrix),
                            WorldToScreen(ptB, modelViewProjMatrix), col,
                            thickness);
      }
    }
  }
}

void DecomposeMatrix(const float *matrix, float *translation, float *rotation,
                     float *scale) {
  IM_ASSERT(matrix);

  auto mat = *reinterpret_cast<const glm::mat4 *>(matrix);
  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis) {
    if (scale) scale[axis] = glm::length(mat[axis]);
    mat[axis] = glm::normalize(mat[axis]);
  }
  if (rotation) {
    rotation[0] = glm::degrees(glm::atan(mat[1][2], mat[2][2]));
    rotation[1] = glm::degrees(glm::atan(
      -mat[0][2], glm::sqrt(mat[1][2] * mat[1][2] + mat[2][2] * mat[2][2])));
    rotation[2] = glm::degrees(glm::atan(mat[0][1], mat[0][0]));
  }
  if (translation)
    for (int i = 0; i < 3; ++i)
      translation[i] = mat[3][i];
}
void RecomposeMatrix(const float *translation, const float *rotation,
                     const float *scale, float *matrix) {
  IM_ASSERT(matrix && translation && rotation && scale);

  glm::mat4 mat{ 1.0f };

  // Rotate
  for (int i = 2; i >= 0; --i)
    mat *= glm::rotate(glm::radians(rotation[i]), kUnitDirections[i]);
  // Scale
  for (ImGuizmoAxis axis = 0; axis < ImGuizmoAxis_COUNT; ++axis) {
    const auto validScale =
      glm::abs(scale[axis]) < kEpsilon ? 0.001f : scale[axis];
    mat[axis] *= validScale;
  }
  // Translate
  mat[3] = glm::vec4{ glm::make_vec3(translation), 1.0f };
  *reinterpret_cast<glm::mat4 *>(matrix) = std::move(mat);
}

}; // namespace ImGuizmo

using namespace ImGuizmo;

//-----------------------------------------------------------------------------
// [SECTION] ImGuizmoContext METHODS
//-----------------------------------------------------------------------------

ImGuizmoContext::~ImGuizmoContext() {
  for (int i = 0; i < Gizmos.Size; ++i)
    delete Gizmos[i];

  Gizmos.clear();
  GizmosById.Clear();
  CurrentGizmo = nullptr;
}

float ImGuizmoContext::GetAspectRatio() const {
  return Viewport.GetWidth() / Viewport.GetHeight();
}

//-----------------------------------------------------------------------------
// [SECTION] ImGuizmoWidget METHODS
//-----------------------------------------------------------------------------

float ImGuizmoWidget::ComputeAngleOnPlane() const {
  const ImGuizmoContext &g{ GImGuizmo };

  const float length{ IntersectRayPlane(g.Ray, TranslationPlane) };
  const glm::vec3 localPos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - ModelMatrix[3].xyz()) };

  const glm::vec3 perpendicularVec{ glm::normalize(
    glm::cross(RotationVectorSource, TranslationPlane.xyz())) };

  const float acosAngle{ glm::clamp(glm::dot(localPos, RotationVectorSource),
                                    -1.0f, 1.0f) };
  float angle{ glm::acos(acosAngle) };
  angle *= (glm::dot(localPos, perpendicularVec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}