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

/*
  TODO:
  - Reduce cognitive complexity
  - Refactor GetTranslateType
    - ComputeTripod ...
  - Refactor GetScaleType
*/

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

//
//
//

using ImGuizmoMode = int;
using ImGuizmoAxis = int;
using ImGuizmoMoveType = int;

enum ImGuizmoAxis_ {
  ImGuizmoAxis_X,
  ImGuizmoAxis_Y,
  ImGuizmoAxis_Z,

  ImGuizmoAxis_COUNT
};

enum ImGuizmoPlane_ {
  ImGuizmoPlane_YZ,
  ImGuizmoPlane_ZX,
  ImGuizmoPlane_XY,

  ImGuizmoPlane_COUNT
};

enum ImGuizmoMoveType_ {
  ImGuizmoMoveType_None,

  ImGuizmoMoveType_MoveX,
  ImGuizmoMoveType_MoveY,
  ImGuizmoMoveType_MoveZ,
  ImGuizmoMoveType_MoveYZ,
  ImGuizmoMoveType_MoveZX,
  ImGuizmoMoveType_MoveXY,
  ImGuizmoMoveType_MoveScreen,

  ImGuizmoMoveType_RotateX,
  ImGuizmoMoveType_RotateY,
  ImGuizmoMoveType_RotateZ,
  ImGuizmoMoveType_RotateScreen,

  ImGuizmoMoveType_ScaleX,
  ImGuizmoMoveType_ScaleY,
  ImGuizmoMoveType_ScaleZ,
  ImGuizmoMoveType_ScaleXYZ,

  ImGuizmoMoveType_COUNT
};

struct ImGuizmoRay {
  glm::vec3 start{ 0.0f };
  glm::vec3 end{ 0.0f };
  glm::vec3 direction{ 0.0f };
};
struct ImGuizmoPlane {
  glm::vec3 normal{ 0.0f };
  float distance;
};

enum ImGuizmoOperation_ {
  ImGuizmoOperation_Translate,
  ImGuizmoOperation_Rotate,
  ImGuizmoOperation_Scale,

  ImGuizmoOperation_Bounds,

  ImGuizmoOperation_COUNT
};

struct ImGuizmoWidget {
  bool inUse{ false };

  ImGuizmoMode_ mode{ ImGuizmoMode_Local };
  ImGuizmoOperation_ operation{ ImGuizmoOperation_Translate };
  ImGuizmoMoveType currentMoveType{ ImGuizmoMoveType_None };

  glm::vec2 origin{ 0.0f };

  struct {
    glm::vec2 min, max;
  } aabb;
  float ringRadius{ 0.0f };
};

static ImGuizmoWidget *CreateNewGizmo(const char *name /*flags?*/) {
  bool OverGizmoOrigin();
  bool OverAlignedRing();
}

//-----------------------------------------------------------------------------
// [SECTION] CONTEXT
//-----------------------------------------------------------------------------

struct ImGuizmoContext {
  bool IsInsideViewport(const glm::vec2 &point) const;

  bool OverGizmoOrigin(const glm::vec2 &mousePosition) const;
  bool OverCameraAlignedRing(const glm::vec2 &mousePosition) const;

  // ---

  bool enabled{ true };

  ImDrawList *drawList{ nullptr };
  ImGuizmoStyle style{};

  ImRect viewport;
  float aspectRatio{ 1.0f };

  struct {
    bool isOrtho{ false };

    glm::mat4 viewMatrix;
    glm::mat4 projectionMatrix;
    glm::mat4 viewProjectionMatrix;

    glm::vec3 right;
    glm::vec3 up;
    glm::vec3 forward;
    glm::vec3 eye;
  } camera;
  ImGuizmoRay ray{};
  float screenFactor;

  glm::vec2 dragOrigin;

  glm::mat4 modelMatrix;
  glm::mat4 modelViewProjMatrix;

  glm::mat4 modelSourceMatrix;
  glm::mat4 inversedModelMatrix;

  struct {
    bool IsPresentlyUsed() const {
      return inUse;
    }

    bool inUse{ false };

    ImGuizmoMode_ mode{ ImGuizmoMode_Local };
    ImGuizmoMoveType currentMoveType{ ImGuizmoMoveType_None };

    glm::vec2 origin{ 0.0f }; // in screen space

    float ringRadius{ 0.0f };

    glm::vec3 modelScaleOrigin{ 1.0f };
    glm::vec3 modelRelativeOrigin{ 0.0f };

  } gizmo;
  struct {
    bool inUse{ false };

    glm::vec3 localPivot;
    glm::vec3 pivot;
    glm::vec3 anchor;
    glm::vec4 plane;
    int bestAxis;
    int axis[2];
    glm::mat4 matrix;
  } bounds;

  // translation
  glm::vec4 translationPlane;
  glm::vec3 translationPlaneOrigin;
  glm::vec3 dragTranslationOrigin;
  glm::vec3 lastTranslationDelta;

  // rotation
  glm::vec3 rotationVectorSource;
  float rotationAngle; // in radians
  float rotationAngleOrigin;

  // scale
  glm::vec3 scale{ 1.0f };
  glm::vec3 scaleValueOrigin{ 1.0f };
  glm::vec3 lastScale{ 1.0f };

  // save axis factor when using gizmo
  //bool belowAxisLimit[3];
  //bool belowPlaneLimit[3];
};
static ImGuizmoContext GImGuizmo{};

//-----------------------------------------------------------------------------
// [SECTION] STYLING
//-----------------------------------------------------------------------------

namespace ImGuizmo {
ImGuizmoStyle &GetStyle() { return GImGuizmo.style; }
} // namespace ImGuizmo

void StyleColorsDefault(ImGuizmoStyle *dst) {
  ImGuizmoStyle *style{ dst ? dst : &ImGuizmo::GetStyle() };
  ImVec4 *colors{ style->Colors };

  // auto col = ImGui::ColorConvertU32ToFloat4(0x801080FF);

  colors[ImGuizmoCol_Inactive] = ImVec4{ 0.600f, 0.600f, 0.600f, 0.600f };
  colors[ImGuizmoCol_Selection] = ImVec4{ 1.00f, 0.501f, 0.062f, 1.00f };

  colors[ImGuizmoCol_Text] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };
  colors[ImGuizmoCol_TextShadow] = ImVec4{ 0.00f, 0.00f, 0.00f, 1.00f };

  colors[ImGuizmoCol_AxisX] = ImVec4{ 0.764f, 0.156f, 0.117f, 1.00f };
  colors[ImGuizmoCol_AxisY] = ImVec4{ 0.411f, 0.686f, 0.098f, 1.00f };
  colors[ImGuizmoCol_AxisZ] = ImVec4{ 0.215f, 0.509f, 0.823f, 1.00f };

  colors[ImGuizmoCol_PlaneYZ] = ImVec4{ 0.764f, 0.156f, 0.117f, 0.60f };
  colors[ImGuizmoCol_PlaneZX] = ImVec4{ 0.411f, 0.686f, 0.098f, 0.60f };
  colors[ImGuizmoCol_PlaneXY] = ImVec4{ 0.215f, 0.509f, 0.823f, 0.60f };
}
ImGuizmoStyle::ImGuizmoStyle() { StyleColorsDefault(this); }

static ImU32 GetColorU32(ImGuiCol idx, float alpha_mul = 1.0f) {
  const ImGuizmoStyle &style{ GImGuizmo.style };
  ImVec4 c{ style.Colors[idx] };
  c.w *= style.Alpha * alpha_mul;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static ImU32 GetColorU32(const ImVec4 &col) {
  const ImGuizmoStyle &style{ GImGuizmo.style };
  ImVec4 c{ col };
  c.w *= style.Alpha;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static const ImVec4 &GetStyleColorVec4(ImGuiCol idx) {
  const ImGuizmoStyle &style{ GImGuizmo.style };
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

  glm::vec2 screenPosition = glm::vec2{ temp } + 0.5f;
  screenPosition.y = 1.0f - screenPosition.y;
  screenPosition *= size;
  screenPosition += position;
  return screenPosition;
}
static glm::vec2 WorldToScreen(const glm::vec3 &worldPos,
                               const glm::mat4 &matrix) {
  return WorldToScreen(worldPos, matrix, GImGuizmo.viewport.GetTL(),
                       GImGuizmo.viewport.GetBR());
}

static ImGuizmoRay ScreenToWorldRay(const glm::vec2 &mousePosition,
                                    const glm::mat4 &viewProjMatrix,
                                    const glm::vec2 &position,
                                    const glm::vec2 &size) {
  const float x{ ((mousePosition.x - position.x) / size.x) * 2.0f - 1.0f };
  const float y{ (1.0f - ((mousePosition.y - position.y) / size.y)) * 2.0f -
                 1.0f };

  const glm::mat4 inversedViewProj{ glm::inverse(viewProjMatrix) };
  glm::vec4 rayStartWorldSpace{ inversedViewProj *
                                glm::vec4{ x, y, 0.0f, 1.0f } };
  rayStartWorldSpace *= 1.0f / rayStartWorldSpace.w;
  glm::vec4 rayEndWorldSpace{ inversedViewProj *
                              glm::vec4{ x, y, 1.0f - FLT_EPSILON, 1.0f } };
  rayEndWorldSpace *= 1.0f / rayEndWorldSpace.w;
  return ImGuizmoRay{ rayStartWorldSpace, rayEndWorldSpace,
                      glm::normalize(rayEndWorldSpace - rayStartWorldSpace) };
}


static glm::vec4 BuildPlane(const glm::vec3 &point, const glm::vec3 &normal) {
  const auto n = glm::normalize(normal);
  return glm::vec4{ n, glm::dot(n, point) };
}
static float DistanceToPlane(const glm::vec3 &point, const glm::vec4 &plane) {
  return glm::dot(glm::vec3{ plane }, point) + plane.w;
}
static float IntersectRayPlane(const ImGuizmoRay &ray, const glm::vec4 &plane) {
  const float num{ glm::dot(glm::vec3{ plane }, ray.start) - plane.w };
  const float denom{ glm::dot(glm::vec3{ plane }, ray.direction) };

  // Normal is orthogonal to vector, cantt intersect
  if (glm::abs(denom) < FLT_EPSILON) return -1.0f;
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

  auto startOfSegment = g.modelViewProjMatrix * glm::vec4{ start, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(startOfSegment.w) > FLT_EPSILON)
    startOfSegment *= 1.0f / startOfSegment.w;

  auto endOfSegment = g.modelViewProjMatrix * glm::vec4{ end, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(endOfSegment.w) > FLT_EPSILON)
    endOfSegment *= 1.0f / endOfSegment.w;

  glm::vec2 clipSpaceAxis{ endOfSegment - startOfSegment };
  clipSpaceAxis.y /= g.aspectRatio;
  return glm::length(clipSpaceAxis);
}

static float GetParallelogram(const glm::vec4 &ptO, const glm::vec4 &ptA,
                              const glm::vec4 &ptB) {
  const ImGuizmoContext &g{ GImGuizmo };

  glm::vec4 points[]{ ptO, ptA, ptB };
  for (int i = 0; i < 3; ++i) {
    points[i].w = 1.0f;
    points[i] = g.modelViewProjMatrix * points[i];
    // Check for axis aligned with camera direction
    if (glm::abs(points[i].w) > FLT_EPSILON) {
      points[i] *= 1.0f / points[i].w;
    }
  }
  glm::vec4 segA{ points[1] - points[0] };
  segA.y /= g.aspectRatio;
  glm::vec4 segB{ points[2] - points[0] };
  segB.y /= g.aspectRatio;

  const auto segAOrtho =
    glm::normalize(glm::vec4{ -segA.y, segA.x, 0.0f, 0.0f });
  const float dt{ glm::dot(segAOrtho, segB) };
  const float surface{ glm::length(segA) * glm::abs(dt) };
  return surface;
}

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES
//-----------------------------------------------------------------------------

static void ComputeSnap(float &value, float snap) {
  if (snap <= FLT_EPSILON) return;

#if 0
  float modulo{ fmodf(value, snap) };
#else
  float modulo{ glm::fmod(value, snap) };
#endif

  constexpr float kSnapTension{ 0.5f };
  const float moduloRatio{ glm::abs(modulo) / snap };
  if (moduloRatio < kSnapTension) {
    value -= modulo;
  } else if (moduloRatio > (1.0f - kSnapTension)) {
    value = value - modulo + snap * ((value < 0.0f) ? -1.0f : 1.0f);
  }
}
static void ComputeSnap(glm::vec3 &value, const float *snap) {
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    ComputeSnap(value[axis], snap[axis]);
}

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES
//-----------------------------------------------------------------------------

static const char *to_str(ImGuizmoOperation_ operation) {
  switch (operation) {
  case ImGuizmoOperation_Translate:
    return "Translate";
  case ImGuizmoOperation_Rotate:
    return "Rotate";
  case ImGuizmoOperation_Scale:
    return "Scale";

  case ImGuizmoOperation_Bounds:
    return "Bounds";
  }

  return nullptr;
}
static const char *to_str(ImGuizmoMoveType_ moveType) {
  switch (moveType) {
  case ImGuizmoMoveType_None:
    return "None";
  case ImGuizmoMoveType_MoveX:
    return "MoveX";
  case ImGuizmoMoveType_MoveY:
    return "MoveY";
  case ImGuizmoMoveType_MoveZ:
    return "MoveZ";
  case ImGuizmoMoveType_MoveYZ:
    return "MoveYZ";
  case ImGuizmoMoveType_MoveZX:
    return "MoveZX";
  case ImGuizmoMoveType_MoveXY:
    return "MoveXY";
  case ImGuizmoMoveType_MoveScreen:
    return "MoveScreen";

  case ImGuizmoMoveType_RotateX:
    return "RotateX";
  case ImGuizmoMoveType_RotateY:
    return "RotateY";
  case ImGuizmoMoveType_RotateZ:
    return "RotateZ";
  case ImGuizmoMoveType_RotateScreen:
    return "RotateScreen";

  case ImGuizmoMoveType_ScaleX:
    return "ScaleX";
  case ImGuizmoMoveType_ScaleY:
    return "ScaleY";
  case ImGuizmoMoveType_ScaleZ:
    return "ScaleZ";
  case ImGuizmoMoveType_ScaleXYZ:
    return "ScaleXYZ";
  }

  return nullptr;
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

static bool IsTranslation(ImGuizmoMoveType moveType) {
  return moveType >= ImGuizmoMoveType_MoveX &&
         moveType <= ImGuizmoMoveType_MoveScreen;
}
static bool IsRotation(ImGuizmoMoveType moveType) {
  return moveType >= ImGuizmoMoveType_RotateX &&
         moveType <= ImGuizmoMoveType_RotateScreen;
}
static bool IsScale(ImGuizmoMoveType moveType) {
  return moveType >= ImGuizmoMoveType_ScaleX &&
         moveType <= ImGuizmoMoveType_ScaleXYZ;
}

static bool CanTranslate(ImGuizmoMoveType moveType) {
  return (moveType == ImGuizmoMoveType_None || IsTranslation(moveType));
}
static bool CanRotate(ImGuizmoMoveType moveType) {
  return (moveType == ImGuizmoMoveType_None || IsRotation(moveType));
}
static bool CanScale(ImGuizmoMoveType moveType) {
  return moveType == ImGuizmoMoveType_None || IsScale(moveType);
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------


static const int kTranslationInfoIndex[]{ 0, 0, 0, 1, 0, 0, 2, 0, 0, 1, 2,
                                          0, 0, 2, 0, 0, 1, 0, 0, 1, 2 };

static void DrawText(const glm::vec2 &position, const char *text) {
  const ImGuizmoContext &g{ GImGuizmo };
  g.drawList->AddText(position + 15.0f, GetColorU32(ImGuizmoCol_TextShadow),
                      text);
  g.drawList->AddText(position + 14.0f, GetColorU32(ImGuizmoCol_Text), text);
}

static void DrawTranslationInfo(const glm::vec2 &position,
                                ImGuizmoMoveType moveType,
                                const glm::vec3 &deltaInfo) {
  static const char *kTranslationInfoMask[]{ "X : %5.3f",
                                             "Y : %5.3f",
                                             "Z : %5.3f",
                                             "Y : %5.3f Z : %5.3f",
                                             "X : %5.3f Z : %5.3f",
                                             "X : %5.3f Y : %5.3f",
                                             "X : %5.3f Y : %5.3f Z : %5.3f" };

  const int componentInfoIndex{ (moveType - ImGuizmoMoveType_MoveX) * 3 };
  char translationInfo[512]{};
  ImFormatString(translationInfo, sizeof(translationInfo),
                 kTranslationInfoMask[moveType - ImGuizmoMoveType_MoveX],
                 deltaInfo[kTranslationInfoIndex[componentInfoIndex]],
                 deltaInfo[kTranslationInfoIndex[componentInfoIndex + 1]],
                 deltaInfo[kTranslationInfoIndex[componentInfoIndex + 2]]);
  DrawText(position, translationInfo);
}
static void DrawRotationInfo(const glm::vec2 &position,
                             ImGuizmoMoveType moveType, float angle) {
  static const char *kRotationInfoMask[]{ "X : %5.2f deg %5.2f rad",
                                          "Y : %5.2f deg %5.2f rad",
                                          "Z : %5.2f deg %5.2f rad",
                                          "Screen : %5.2f deg %5.2f rad" };

  char rotationInfo[512]{};
  ImFormatString(rotationInfo, sizeof(rotationInfo),
                 kRotationInfoMask[moveType - ImGuizmoMoveType_RotateX],
                 glm::degrees(angle), angle);
  DrawText(position, rotationInfo);
}
static void DrawScaleInfo(const glm::vec2 &position, ImGuizmoMoveType moveType,
                          const glm::vec3 scaleDisplay) {
  static const char *kScaleInfoMask[]{ "X : %5.2f", "Y : %5.2f", "Z : %5.2f",
                                       "XYZ : %5.2f" };

  char scaleInfo[512]{};
  const int componentInfoIndex{ (moveType - ImGuizmoMoveType_ScaleX) * 3 };
  ImFormatString(scaleInfo, sizeof(scaleInfo),
                 kScaleInfoMask[moveType - ImGuizmoMoveType_ScaleX],
                 scaleDisplay[kTranslationInfoIndex[componentInfoIndex]]);
  DrawText(position, scaleInfo);
}

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES
//-----------------------------------------------------------------------------

static constexpr auto PI = glm::pi<float>();

const glm::vec3 kReferenceUp{ 0.0f, 1.0f, 0.0f };
static const glm::vec3 kUnitDirection[3]{ { 1.0f, 0.0f, 0.0f },
                                          { 0.0f, 1.0f, 0.0f },
                                          { 0.0f, 0.0f, 1.0f } };

constexpr auto kGrayColor = 0x999999;
constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

constexpr auto kLightGrayColor = 0xffdfdfdf;


namespace priv {

constexpr float kQuadMin{ 0.5f };
constexpr float kQuadMax{ 0.8f };
static const float kQuadUV[]{ kQuadMin, kQuadMin, kQuadMin, kQuadMax,
                              kQuadMax, kQuadMax, kQuadMax, kQuadMin };

// ---

constexpr float kRotationRingSize{ 0.06f };
constexpr int kArcSegmentCount{ 64 };


static bool CanActivate() {
  if (ImGui::IsMouseClicked(0) && !ImGui::IsAnyItemHovered() &&
      !ImGui::IsAnyItemActive()) {
    return true;
  }
  return false;
}

static bool IsAxisVisible(const glm::vec3 &dirAxis) {
  constexpr float kVisibilityThreshold{ 0.02f };
  const float axisLengthInClipSpace{ GetSegmentLengthClipSpace(
    glm::vec3{ 0.0f }, dirAxis * GImGuizmo.screenFactor) };
  return axisLengthInClipSpace > kVisibilityThreshold;
}

static bool IsPlaneVisible(const glm::vec3 &dir1, const glm::vec3 &dir2) {
  constexpr float kVisibilityThreshold{ 0.0025f };
  const float paraSurf{ GetParallelogram(
    glm::vec4{ 0.0f }, glm::vec4{ dir1, 0.0f } * GImGuizmo.screenFactor,
    glm::vec4{ dir2, 0.0f } * GImGuizmo.screenFactor) };
  return paraSurf > kVisibilityThreshold;
}

static ImGuizmoMoveType GetTranslateType(const glm::vec2 &mousePosition) {
  const ImGuizmoContext &g{ GImGuizmo };

  if (g.OverGizmoOrigin(mousePosition))
    return ImGuizmoMoveType_MoveScreen;

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 modelPosition{ g.modelMatrix[3] };
    const glm::vec3 dirAxis{ g.modelMatrix *
                             glm::vec4{ kUnitDirection[axis], 0.0f } };
    const float length{ IntersectRayPlane(g.ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 mousePosOnPlane{ g.ray.start + g.ray.direction * length };
    const glm::vec2 mousePosOnPlaneInSS{ WorldToScreen(
      mousePosOnPlane, g.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.screenFactor * 0.1f,
      g.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.screenFactor,
      g.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      mousePosOnPlaneInSS, axisStartOnScreen, axisEndOnScreen) };
    constexpr float kTolerance{ 12.0f };
    if (glm::length(closestPointOnAxis - mousePosOnPlaneInSS) < kTolerance)
      return ImGuizmoMoveType_MoveX + axis;

    // ---

    const glm::vec3 planeDir1{
      g.modelMatrix * glm::vec4{ kUnitDirection[(axis + 1) % 3], 0.0f }
    };
    const float dx{ glm::dot(planeDir1, (mousePosOnPlane - modelPosition) *
                                          (1.0f / g.screenFactor)) };
    const glm::vec3 planeDir2{
      g.modelMatrix * glm::vec4{ kUnitDirection[(axis + 2) % 3], 0.0f }
    };
    const float dy{ glm::dot(planeDir2, (mousePosOnPlane - modelPosition) *
                                          (1.0f / g.screenFactor)) };

    if (IsPlaneVisible(planeDir1, planeDir2) && dx >= kQuadUV[0] &&
        dx <= kQuadUV[4] && dy >= kQuadUV[1] && dy <= kQuadUV[3]) {
      return ImGuizmoMoveType_MoveYZ + axis;
    }
  }

  return ImGuizmoMoveType_None;
}

static bool IsOverRotationAxis(const glm::vec2 &mousePosition,
                               ImGuizmoAxis axis) {
  const ImGuizmoContext &g{ GImGuizmo };

  const glm::vec4 pickupPlane{ BuildPlane(g.modelMatrix[3],
                                          g.modelMatrix[axis]) };
  const float length{ IntersectRayPlane(g.ray, pickupPlane) };
  const glm::vec3 localPos{ glm::normalize(
    g.ray.start + g.ray.direction * length - glm::vec3{ g.modelMatrix[3] }) };

  // ... ?
  if (glm::dot(localPos, g.ray.direction) > FLT_EPSILON) return false;

  const glm::vec3 idealPosOnCircle{ glm::mat3{ g.inversedModelMatrix } *
                                    localPos };
  const glm::vec2 idealPosOnCircleScreen{ WorldToScreen(
    idealPosOnCircle * g.screenFactor, g.modelViewProjMatrix) };

  constexpr float kTolerance{ 8.0f };
  const glm::vec2 distanceOnScreen{ idealPosOnCircleScreen - mousePosition };
  return glm::length(distanceOnScreen) < kTolerance;
}

static ImGuizmoMoveType GetRotateType(const glm::vec2 &mousePosition) {
  if (GImGuizmo.OverCameraAlignedRing(mousePosition))
    return ImGuizmoMoveType_RotateScreen;

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    if (IsOverRotationAxis(mousePosition, axis))
      return ImGuizmoMoveType_RotateX + axis;

  return ImGuizmoMoveType_None;
}

static ImGuizmoMoveType GetScaleType(const glm::vec2 &mousePosition) {
  const ImGuizmoContext &g{ GImGuizmo };

  if (g.OverGizmoOrigin(mousePosition))
    return ImGuizmoMoveType_ScaleXYZ;

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 modelPosition{ g.modelMatrix[3] };

    const glm::vec3 dirAxis{ g.modelMatrix *
                             glm::vec4{ kUnitDirection[axis], 0.0f } };
    const float length{ IntersectRayPlane(g.ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 mousePosOnPlane{ g.ray.start + g.ray.direction * length };
    const glm::vec2 mousePosOnPlaneInSS{ WorldToScreen(
      mousePosOnPlane, g.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.screenFactor * 0.1f,
      g.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.screenFactor,
      g.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      mousePosOnPlaneInSS, axisStartOnScreen, axisEndOnScreen) };
    constexpr float kTolerance{ 8.0f };
    if (glm::length(closestPointOnAxis - mousePosOnPlaneInSS) < kTolerance)
      return ImGuizmoMoveType_ScaleX + axis;
  }

  return ImGuizmoMoveType_None;
}

static void ComputeColors(ImU32 colors[7], ImGuizmoMoveType moveType,
                          ImGuizmoOperation_ operation) {
  if (!GImGuizmo.enabled) {
    for (int i = 0; i < 7; ++i)
      colors[i] = GetColorU32(ImGuizmoCol_Inactive);
    return;
  }

  const auto kWhiteColor = 0xFFFFFFFF;
  const auto kSelectionColor = GetColorU32(ImGuizmoCol_Selection, 0.541f);
  switch (operation) {
  case ImGuizmoOperation_Translate:
    colors[0] =
      (moveType == ImGuizmoMoveType_MoveScreen) ? kSelectionColor : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (moveType == (ImGuizmoMoveType_MoveX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
      colors[i + 4] = (moveType == (ImGuizmoMoveType_MoveYZ + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_PlaneYZ + i);
      colors[i + 4] = (moveType == ImGuizmoMoveType_MoveScreen)
                        ? kSelectionColor
                        : colors[i + 4];
    }
    break;
  case ImGuizmoOperation_Rotate:
    colors[0] = (moveType == ImGuizmoMoveType_RotateScreen) ? kSelectionColor
                                                            : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (moveType == (ImGuizmoMoveType_RotateX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
    }
    break;
  case ImGuizmoOperation_Scale:
    colors[0] =
      (moveType == ImGuizmoMoveType_ScaleXYZ) ? kSelectionColor : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (moveType == (ImGuizmoMoveType_ScaleX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
    }
    break;

  case ImGuizmoOperation_Bounds:
    break;
  }
}

static float ComputeAngleOnPlane() {
  const float length{ IntersectRayPlane(GImGuizmo.ray,
                                        GImGuizmo.translationPlane) };
  const glm::vec3 localPos{ glm::normalize(
    GImGuizmo.ray.start + GImGuizmo.ray.direction * length -
    glm::vec3{ GImGuizmo.modelMatrix[3] }) };

  const glm::vec3 perpendicularVec{ glm::normalize(glm::cross(
    GImGuizmo.rotationVectorSource, glm::vec3{ GImGuizmo.translationPlane })) };

  const float acosAngle{ glm::clamp(
    glm::dot(localPos, GImGuizmo.rotationVectorSource), -1.0f, 1.0f) };
  float angle{ glm::acos(acosAngle) };
  angle *= (glm::dot(localPos, perpendicularVec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}

static void DrawTranslationGizmo(ImGuizmoMoveType moveType) {
  const ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Translate);

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 dirAxis { kUnitDirection[axis] };
    if (IsAxisVisible(dirAxis)) {
      const glm::vec2 tail{ WorldToScreen(dirAxis * 0.1f * g.screenFactor,
                                          g.modelViewProjMatrix) };
      const glm::vec2 head{ WorldToScreen(dirAxis * g.screenFactor,
                                          g.modelViewProjMatrix) };

      constexpr float kLineThickness{ 3.0f };
      g.drawList->AddLine(tail, head, colors[axis + 1], kLineThickness);
      constexpr float kArrowheadSize{ kLineThickness * 2.0f };
      const glm::vec2 dir{ glm::normalize(g.gizmo.origin - head) *
                           kArrowheadSize };
      const glm::vec2 ortogonalDir{ dir.y, -dir.x };
      const glm::vec2 a{ head + dir };
      g.drawList->AddTriangleFilled(head - dir, a + ortogonalDir,
                                    a - ortogonalDir, colors[axis + 1]);
    }

    const glm::vec3 planeDir1{ kUnitDirection[(axis + 1) % 3] };
    const glm::vec3 planeDir2{ kUnitDirection[(axis + 2) % 3] };
    if (IsPlaneVisible(planeDir1, planeDir2)) {
      ImVec2 screenQuadPts[4]{};
      for (int i = 0; i < 4; ++i) {
        const glm::vec3 cornerWorldSpace{ (planeDir1 * kQuadUV[i * 2] +
                                           planeDir2 * kQuadUV[i * 2 + 1]) *
                                          g.screenFactor };
        screenQuadPts[i] =
          WorldToScreen(cornerWorldSpace, g.modelViewProjMatrix);
      }

      g.drawList->AddConvexPolyFilled(screenQuadPts, 4, colors[axis + 4]);
      constexpr float kQuadBorder{ 1.5f };
      g.drawList->AddPolyline(screenQuadPts, 4,
                              GetColorU32(ImGuizmoCol_AxisX + axis), true,
                              kQuadBorder);
    }
  }

  const float kCircleRadius{ 6.0f };
  if (moveType != ImGuizmoMoveType_None && g.gizmo.inUse) {
    const glm::vec2 tail{ WorldToScreen(g.dragTranslationOrigin,
                                        g.camera.viewProjectionMatrix) };
    const glm::vec3 &modelPosition{ g.modelMatrix[3] };
    const glm::vec2 head{ WorldToScreen(modelPosition,
                                        g.camera.viewProjectionMatrix) };
    const glm::vec2 diff{ glm::normalize(head - tail) *
                          (kCircleRadius - 1.0f) };

    constexpr auto kTranslationLineColor = 0xAAAAAAAA;
    constexpr float kMargin{ 1.5f };
    g.drawList->AddCircle(tail, kCircleRadius + kMargin, kTranslationLineColor);
    g.drawList->AddCircle(head, kCircleRadius + kMargin, kTranslationLineColor);
    g.drawList->AddLine(tail + diff, head - diff, kTranslationLineColor, 2.0f);

    const glm::vec3 deltaInfo{ modelPosition - g.dragTranslationOrigin };
    DrawTranslationInfo(head, moveType, deltaInfo);
  }

  g.drawList->AddCircleFilled(g.gizmo.origin, kCircleRadius, colors[0], 32);
}

static void DrawRotationGizmo(ImGuizmoMoveType moveType) {
  ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Rotate);

  const glm::vec3 &modelPosition{ g.modelMatrix[3] };
  glm::vec3 cameraToModelNormalized{
    g.camera.isOrtho ? glm::inverse(g.camera.viewMatrix)[2]
                     : glm::normalize(modelPosition - g.camera.eye)
  };
  // Always face to camera
  cameraToModelNormalized =
    g.inversedModelMatrix * glm::vec4{ cameraToModelNormalized, 0.0f };

  g.gizmo.ringRadius = kRotationRingSize * g.viewport.GetHeight();

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const float angleStart{ (glm::atan(
                              cameraToModelNormalized[(4 - axis) % 3],
                              cameraToModelNormalized[(3 - axis) % 3])) +
                            PI * 0.5f };

    ImVec2 circlePos[kArcSegmentCount]{};
    for (int i = 0; i < kArcSegmentCount; ++i) {
      const float ng{ angleStart +
                      PI * (static_cast<float>(i) / kArcSegmentCount) };
      const glm::vec3 axisPos{ glm::cos(ng), glm::sin(ng), 0.0f };
      const auto pos = glm::vec3{ axisPos[axis], axisPos[(axis + 1) % 3],
                                  axisPos[(axis + 2) % 3] } *
                       g.screenFactor;
      circlePos[i] = WorldToScreen(pos, g.modelViewProjMatrix);
    }

    const float radiusAxis{ glm::length(g.gizmo.origin -
                                        glm::vec2{ circlePos[0] }) };
    if (radiusAxis > g.gizmo.ringRadius) g.gizmo.ringRadius = radiusAxis;
    g.drawList->AddCircleFilled(circlePos[0], 6.5f,
                                colors[ImGuizmoAxis_COUNT - axis], 32);
    g.drawList->AddPolyline(circlePos, kArcSegmentCount,
                            colors[ImGuizmoAxis_COUNT - axis], false, 2.0f);
  }

  // Circle parallel to view
  g.drawList->AddCircle(g.gizmo.origin, g.gizmo.ringRadius, colors[0],
                        kArcSegmentCount, g.style.RotationRingThickness);

  if (moveType != ImGuizmoMoveType_None && g.gizmo.inUse) {
    ImVec2 circlePos[kArcSegmentCount + 1]{ g.gizmo.origin };
    for (int i = 1; i < kArcSegmentCount; ++i) {
      const float ng{ g.rotationAngle *
                      (static_cast<float>(i - 1) / (kArcSegmentCount - 1)) };
      const glm::mat3 rotateVectorMatrix{ glm::rotate(
        ng, glm::vec3{ g.translationPlane }) };
      glm::vec3 pos{ rotateVectorMatrix * g.rotationVectorSource };
      pos *= g.screenFactor;
      circlePos[i] =
        WorldToScreen(pos + modelPosition, g.camera.viewProjectionMatrix);
    }

    // Draw inner part ...
    g.drawList->AddConvexPolyFilled(circlePos, kArcSegmentCount,
                                  GetColorU32(ImGuizmoCol_Selection, 0.541f));
    // ... and outline
    g.drawList->AddPolyline(circlePos, kArcSegmentCount,
                          GetColorU32(ImGuizmoCol_Selection), true, 2.0f);

    DrawRotationInfo(circlePos[1], moveType, g.rotationAngle);
  }
}

static void DrawScaleGizmo(ImGuizmoMoveType moveType) {
  constexpr auto kCircleRadius = 6.0f; // @todo move to global?

  const ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Scale);

  const glm::vec3 scaleDisplay{ g.gizmo.inUse ? g.scale : glm::vec3{ 1.0f } };

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 dirAxis{ kUnitDirection[axis] };
    if (IsAxisVisible(dirAxis)) {
       const glm::vec2 tail{ WorldToScreen(dirAxis * 0.1f * g.screenFactor,
                                           g.modelViewProjMatrix) };
       const glm::vec2 head{ WorldToScreen(dirAxis * GImGuizmo.screenFactor,
                                           g.modelViewProjMatrix) };
       const glm::vec2 headScaled{ WorldToScreen(
         (dirAxis * scaleDisplay[axis]) * g.screenFactor,
         g.modelViewProjMatrix) };

      constexpr float kLineThickness{ 3.0f };
       if (moveType != ImGuizmoMoveType_None && g.gizmo.inUse) {
         g.drawList->AddLine(tail, head, kDarkGrayColor, kLineThickness);
         g.drawList->AddCircleFilled(head, kCircleRadius, kDarkGrayColor);
       }

      g.drawList->AddLine(tail, headScaled, colors[axis + 1], kLineThickness);
      g.drawList->AddCircleFilled(headScaled, kCircleRadius, colors[axis + 1]);
      
      /*
      constexpr float kQuadSize{kLineThickness * 2.0f};
      const glm::vec2 dir{ glm::normalize(gContext.gizmo.origin - headScaled) *
                           kQuadSize };
      const glm::vec2 a{ headScaled + dir }, b{ headScaled - dir };
      const ImVec2 points[4]{ 
        a + glm::vec2{  dir.y, -dir.x },
        a - glm::vec2{  dir.y, -dir.x },
        b + glm::vec2{ -dir.y,  dir.x },
        b - glm::vec2{ -dir.y,  dir.x }
      };
      drawList->AddConvexPolyFilled(points, 4, colors[axis + 1]);
      */      
    }
  }

  // Draw circle at the begining of axes (center)
  g.drawList->AddCircleFilled(g.gizmo.origin, kCircleRadius, colors[0], 32);

  if (moveType != ImGuizmoMoveType_None && g.gizmo.inUse)
    DrawScaleInfo(g.gizmo.origin, moveType, scaleDisplay);
}

static ImGuizmoMoveType BeginTranslation() {
  ImGuizmoContext &g{ GImGuizmo };

  const glm::vec2 &mousePosition{ ImGui::GetIO().MousePos };
  ImGuizmoMoveType moveType{ GetTranslateType(mousePosition) };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.gizmo.inUse = true;
    g.gizmo.currentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.modelMatrix[3] };
    const glm::vec3 cameraToModelNormalized{ glm::normalize(modelPosition -
                                                            g.camera.eye) };
    glm::vec3 movePlaneNormal[]{
      g.modelMatrix[0], g.modelMatrix[1], g.modelMatrix[2],
      g.modelMatrix[0], g.modelMatrix[1], g.modelMatrix[2],
      -g.camera.forward
    };

    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
      const glm::vec3 orthoVector{ glm::cross(movePlaneNormal[axis],
                                              cameraToModelNormalized) };
      movePlaneNormal[axis] =
        glm::normalize(glm::cross(movePlaneNormal[axis], orthoVector));
    }

    g.translationPlane = BuildPlane(
      modelPosition, movePlaneNormal[moveType - ImGuizmoMoveType_MoveX]);

    const auto length{ IntersectRayPlane(g.ray, g.translationPlane) };
    g.translationPlaneOrigin = g.ray.start + g.ray.direction * length;
    g.dragTranslationOrigin = modelPosition;
    g.gizmo.modelRelativeOrigin =
      (g.translationPlaneOrigin - modelPosition) * (1.0f / g.screenFactor);
  }

  return moveType;
}
static bool ContinueTranslation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                                float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();

  const float length{ glm::abs(IntersectRayPlane(g.ray, g.translationPlane)) };
  const glm::vec3 newPos{ g.ray.start + g.ray.direction * length };
  const glm::vec3 newOrigin{ newPos -
                             g.gizmo.modelRelativeOrigin * g.screenFactor };
  const glm::vec3 &modelPosition{ g.modelMatrix[3] };
  glm::vec3 delta{ newOrigin - modelPosition };

  // 1 axis constraint
  if (g.gizmo.currentMoveType >= ImGuizmoMoveType_MoveX &&
      g.gizmo.currentMoveType <= ImGuizmoMoveType_MoveZ) {
    const int axisIndex{ g.gizmo.currentMoveType -
                         ImGuizmoMoveType_MoveX };
    const glm::vec3 axisValue{ g.modelMatrix[axisIndex] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
  }

  if (snap) {
    glm::vec3 cumulativeDelta{ modelPosition + delta -
                               g.dragTranslationOrigin };
    bool applyRotationLocaly{ g.gizmo.mode == ImGuizmoMode_Local ||
                              moveType == ImGuizmoMoveType_MoveScreen };
    if (applyRotationLocaly) {
      glm::mat4 modelSourceNormalized{ g.modelSourceMatrix };
      for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
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
    delta = g.dragTranslationOrigin + cumulativeDelta - modelPosition;
  }

  bool modified{ false };
  if (delta != g.lastTranslationDelta) modified = true;
  g.lastTranslationDelta = delta;

  const glm::mat4 deltaMatrixTranslation{ glm::translate(delta) };
  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = deltaMatrixTranslation;

  moveType = g.gizmo.currentMoveType;
  matrix = deltaMatrixTranslation * g.modelSourceMatrix;

  return modified;
}

/**
 * @param [in/out] moveType
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in] snap
 */
static bool HandleTranslation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                              float *deltaMatrix, const float *snap) {
  IM_ASSERT(moveType == ImGuizmoMoveType_None || IsTranslation(moveType));

  const ImGuizmoContext &g{ GImGuizmo };

  if (!g.gizmo.inUse) moveType = BeginTranslation();
  if (g.gizmo.inUse)
    return ContinueTranslation(moveType, matrix, deltaMatrix, snap);
  return false;
}

static ImGuizmoMoveType BeginRotation() {
  ImGuizmoContext &g{ GImGuizmo };
  const glm::vec2 &mousePosition{ ImGui::GetIO().MousePos };

  ImGuizmoMoveType moveType{ GetRotateType(mousePosition) };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();

  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.gizmo.inUse = true;
    g.gizmo.currentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.modelMatrix[3] };
    const glm::vec3 rotatePlaneNormal[]{ g.modelMatrix[0], g.modelMatrix[1],
                                         g.modelMatrix[2], -g.camera.forward };

    const bool applyRotationLocaly{ g.gizmo.mode == ImGuizmoMode_Local ||
                                    moveType == ImGuizmoMoveType_RotateScreen };
    if (applyRotationLocaly) {
      g.translationPlane = BuildPlane(
        modelPosition, rotatePlaneNormal[moveType - ImGuizmoMoveType_RotateX]);
    } else {
      g.translationPlane =
        BuildPlane(g.modelSourceMatrix[3],
                   kUnitDirection[moveType - ImGuizmoMoveType_RotateX]);
    }

    const float length{ IntersectRayPlane(g.ray, g.translationPlane) };
    const glm::vec3 localPos{ g.ray.start + g.ray.direction * length -
                              modelPosition };
    g.rotationVectorSource = glm::normalize(localPos);
    g.rotationAngleOrigin = ComputeAngleOnPlane();
  }

  return moveType;
}
static bool ContinueRotation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                             float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();
  g.rotationAngle = ComputeAngleOnPlane();
  
  if (snap) ComputeSnap(g.rotationAngle, glm::radians(snap[0]));

  const glm::vec3 rotationAxisLocalSpace{ glm::normalize(
    glm::mat3{ g.inversedModelMatrix } * glm::vec3{ g.translationPlane }) };
  const glm::mat4 deltaRotation{ glm::rotate(
    g.rotationAngle - g.rotationAngleOrigin, rotationAxisLocalSpace) };

  bool modified{ false };
  if (g.rotationAngle != g.rotationAngleOrigin) modified = true;
  g.rotationAngleOrigin = g.rotationAngle;

  const glm::mat4 scaleOrigin{ glm::scale(g.gizmo.modelScaleOrigin) };

  const bool applyRotationLocaly{ g.gizmo.mode == ImGuizmoMode_Local ||
                                  moveType == ImGuizmoMoveType_RotateScreen };
  if (applyRotationLocaly) {
    matrix = g.modelMatrix * deltaRotation * scaleOrigin;
  } else {
    glm::mat4 result{ g.modelSourceMatrix };
    result[3] = glm::vec4{ glm::vec3{ 0.0f }, 1.0f };
    matrix = deltaRotation * result;
    matrix[3] = g.modelSourceMatrix[3];
  }

  if (deltaMatrix) {
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) =
      g.modelMatrix * deltaRotation * g.inversedModelMatrix;
  }

  moveType = g.gizmo.currentMoveType;
  return modified;
}

/**
 * @param [in/out] moveType
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in] snap Angle in degrees (1 element)
 */
static bool HandleRotation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                           float *deltaMatrix, const float *snap) {
  IM_ASSERT(moveType == ImGuizmoMoveType_None || IsRotation(moveType));

  const ImGuizmoContext &g{ GImGuizmo };
  if (!g.gizmo.inUse) moveType = BeginRotation();
  if (g.gizmo.inUse)
    return ContinueRotation(moveType, matrix, deltaMatrix, snap);
  return false;
}

static ImGuizmoMoveType BeginScale() {
  ImGuizmoContext &g{ GImGuizmo };
  const glm::vec2 mousePosition{ ImGui::GetIO().MousePos };

  ImGuizmoMoveType moveType{ GetScaleType(mousePosition) };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();

  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.gizmo.inUse = true;
    g.gizmo.currentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.modelMatrix[3] };
    const glm::vec3 movePlaneNormal[]{ g.modelMatrix[1], g.modelMatrix[2],
                                       g.modelMatrix[0], g.modelMatrix[2],
                                       g.modelMatrix[1], g.modelMatrix[0],
                                       -g.camera.forward };
    g.translationPlane = BuildPlane(
      modelPosition, movePlaneNormal[moveType - ImGuizmoMoveType_ScaleX]);
    const float length{ IntersectRayPlane(g.ray, g.translationPlane) };
    g.translationPlaneOrigin = g.ray.start + g.ray.direction * length;
    g.dragTranslationOrigin = modelPosition;
    g.scale = glm::vec3{ 1.0f };
    g.gizmo.modelRelativeOrigin =
      (g.translationPlaneOrigin - modelPosition) * (1.0f / g.screenFactor);

    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
      g.scaleValueOrigin[axis] = glm::length(g.modelSourceMatrix[axis]);
    }
    g.dragOrigin = mousePosition;
  }

  return moveType;
}
static bool ContinueScale(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                          float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();
  const float length{ IntersectRayPlane(g.ray, g.translationPlane) };
  const glm::vec3 newPos{ g.ray.start + g.ray.direction * length };
  const glm::vec3 newOrigin{ newPos -
                             g.gizmo.modelRelativeOrigin * g.screenFactor };
  const glm::vec3 &modelPosition{ g.modelMatrix[3] };
  glm::vec3 delta{ newOrigin - modelPosition };

  // 1 axis constraint
  if (g.gizmo.currentMoveType >= ImGuizmoMoveType_ScaleX &&
      g.gizmo.currentMoveType <= ImGuizmoMoveType_ScaleZ) {
    const int axisIndex{ g.gizmo.currentMoveType - ImGuizmoMoveType_ScaleX };
    const glm::vec3 axisValue{ g.modelMatrix[axisIndex] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
    const glm::vec3 baseVector{ g.translationPlaneOrigin - modelPosition };
    const float ratio{ glm::dot(axisValue, baseVector + delta) /
                       glm::dot(axisValue, baseVector) };
    g.scale[axisIndex] = glm::max(ratio, 0.001f);
  } else {
    const float scaleDelta{ (ImGui::GetIO().MousePos.x - g.dragOrigin.x) * 0.01f };
    g.scale = glm::vec3{ glm::max(1.0f + scaleDelta, 0.001f) };
  }

  if (snap) {
    const float scaleSnap[]{ snap[0], snap[0], snap[0] };
    ComputeSnap(g.scale, scaleSnap);
  }

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    g.scale[axis] = glm::max(g.scale[axis], 0.001f);

  bool modified{ false };
  if (g.lastScale != g.scale) modified = true;
  g.lastScale = g.scale;

  glm::mat4 deltaMatrixScale{ glm::scale(g.scale * g.scaleValueOrigin) };
  matrix = g.modelMatrix * deltaMatrixScale;

  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::scale(g.scale);

  moveType = g.gizmo.currentMoveType;
  return modified;
}

/**
 * @param [in/out] moveType
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in] snap
 */
static bool HandleScale(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                        float *deltaMatrix, const float *snap) {
  IM_ASSERT(moveType == ImGuizmoMoveType_None || IsScale(moveType));

  const ImGuizmoContext &g{ GImGuizmo };
  if (!g.gizmo.inUse) moveType = BeginScale();
  if (g.gizmo.inUse)
    return ContinueScale(moveType, matrix, deltaMatrix, snap);
  return false;
}










/**
 * @param [in] bounds
 * @param [out] matrix
 * @param [in] snapValues
 */
static void HandleAndDrawLocalBounds(const float *bounds, glm::mat4 &matrix,
                                     const float *snapValues,
                                     ImGuizmoOperation_ operation) {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImDrawList *drawList{ GImGuizmo.drawList };

  // Compute best projection axis
  glm::vec3 axesWorldDirections[3]{};
  glm::vec3 bestAxisWorldDirection{ 0.0f };
  int axes[3]{ 0 };
  unsigned int numAxes{ 1 };
  axes[0] = GImGuizmo.bounds.bestAxis;
  int bestAxis = axes[0];
  if (!GImGuizmo.bounds.inUse) {
    numAxes = 0;
    float bestDot{ 0.0f };
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; axis++) {
      const glm::vec3 dirPlaneNormalWorld{ glm::normalize(
        GImGuizmo.modelSourceMatrix * glm::vec4{ kUnitDirection[axis], 0.0f }) };

      const float dt{ glm::abs(
        glm::dot(glm::normalize(GImGuizmo.camera.eye -
                                glm::vec3{ GImGuizmo.modelSourceMatrix[3] }),
                 dirPlaneNormalWorld)) };

      if (dt >= bestDot) {
        bestDot = dt;
        bestAxis = axis;
        bestAxisWorldDirection = dirPlaneNormalWorld;
      }
      if (dt >= 0.1f) {
        axes[numAxes] = axis;
        axesWorldDirections[numAxes] = dirPlaneNormalWorld;
        ++numAxes;
      }
    }
  }

  if (numAxes == 0) {
    axes[0] = bestAxis;
    axesWorldDirections[0] = bestAxisWorldDirection;
    numAxes = 1;
  } else if (bestAxis != axes[0]) {
    unsigned int bestIndex{ 0 };
    for (unsigned int i = 0; i < numAxes; i++) {
      if (axes[i] == bestAxis) {
        bestIndex = i;
        break;
      }
    }
    const int tempAxis{ axes[0] };
    axes[0] = axes[bestIndex];
    axes[bestIndex] = tempAxis;
    const glm::vec3 tempDirection{ axesWorldDirections[0] };
    axesWorldDirections[0] = axesWorldDirections[bestIndex];
    axesWorldDirections[bestIndex] = tempDirection;
  }

  for (unsigned int axisIndex = 0; axisIndex < numAxes; ++axisIndex) {
    bestAxis = axes[axisIndex];
    bestAxisWorldDirection = axesWorldDirections[axisIndex];

    const int secondAxis{ (bestAxis + 1) % 3 };
    const int thirdAxis{ (bestAxis + 2) % 3 };

    glm::vec3 aabb[4]{};
    for (int i = 0; i < 4; i++) {
      aabb[i][bestAxis] = 0.0f;
      aabb[i][secondAxis] = bounds[secondAxis + 3 * (i >> 1)];
      aabb[i][thirdAxis] = bounds[thirdAxis + 3 * ((i >> 1) ^ (i & 1))];
    }

    unsigned int anchorAlpha{ GImGuizmo.enabled ? 0xFF000000 : 0x80000000 };

    const glm::mat4 boundsMVP{ GImGuizmo.camera.viewProjectionMatrix *
                               GImGuizmo.modelSourceMatrix };

    for (int i = 0; i < 4; i++) {
      const glm::vec2 bound1{ WorldToScreen(aabb[i], boundsMVP) };
      const glm::vec2 bound2{ WorldToScreen(aabb[(i + 1) % 4], boundsMVP) };
      if (!GImGuizmo.IsInsideViewport(bound1) ||
          !GImGuizmo.IsInsideViewport(bound2)) {
        continue;
      }

      const float boundDistance{ glm::length(bound1 - bound2) };
      auto stepCount = static_cast<int>(boundDistance / 10.0f);
      stepCount = glm::min(stepCount, 1000);
      const float stepLength{ 1.0f / stepCount };
      for (int j = 0; j < stepCount; j++) {
        const float t1{ static_cast<float>(j) * stepLength };
        const float t2{ t1 + stepLength * 0.5f };
        const glm::vec2 tail{ glm::lerp(bound1, bound2, t1) };
        const glm::vec2 head{ glm::lerp(bound1, bound2, t2) };
        drawList->AddLine(tail, head, 0xAAAAAA + anchorAlpha, 2.0f);
      }
      const glm::vec3 midPoint{ (aabb[i] + aabb[(i + 1) % 4]) * 0.5f };
      const glm::vec2 midBound{ WorldToScreen(midPoint, boundsMVP) };
      constexpr float bigAnchorRadius{ 6.0f };
      constexpr float smallAnchorRadius{ 4.0f };
      bool overBigAnchor = ImLengthSqr(bound1 - io.MousePos) <=
                           (bigAnchorRadius * bigAnchorRadius);
      bool overSmallAnchor = ImLengthSqr(midBound - io.MousePos) <=
                             (bigAnchorRadius * bigAnchorRadius);

      ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
      switch (operation) {
      case ImGuizmoOperation_Translate:
        moveType = GetTranslateType(io.MousePos);
        break;
      case ImGuizmoOperation_Rotate:
        moveType = GetRotateType(io.MousePos);
        break;
      case ImGuizmoOperation_Scale:
        moveType = GetScaleType(io.MousePos);
        break;

      case ImGuizmoOperation_Bounds:
        break;
      }

      if (moveType != ImGuizmoMoveType_None) {
        overBigAnchor = false;
        overSmallAnchor = false;
      }

      const auto kSelectionColor = 0x8A1080FF;
      auto bigAnchorColor = (overBigAnchor && GImGuizmo.enabled)
                              ? kSelectionColor
                              : (0xAAAAAA + anchorAlpha);
      auto smallAnchorColor = (overSmallAnchor && GImGuizmo.enabled)
                                ? kSelectionColor
                                : (0xAAAAAA + anchorAlpha);

      drawList->AddCircleFilled(bound1, bigAnchorRadius, 0xFF000000);
      drawList->AddCircleFilled(bound1, bigAnchorRadius - 1.2f, bigAnchorColor);

      drawList->AddCircleFilled(midBound, smallAnchorRadius, 0xFF000000);
      drawList->AddCircleFilled(midBound, smallAnchorRadius - 1.2f,
                                smallAnchorColor);

      int oppositeIndex{ (i + 2) % 4 };
      // Big anchor on corners
      if (!GImGuizmo.bounds.inUse && GImGuizmo.enabled && overBigAnchor &&
          CanActivate()) {
        GImGuizmo.bounds.pivot =
          GImGuizmo.modelSourceMatrix * glm::vec4{ aabb[(i + 2) % 4], 1.0f };
        GImGuizmo.bounds.anchor =
          GImGuizmo.modelSourceMatrix * glm::vec4{ aabb[i], 1.0f };

        GImGuizmo.bounds.plane =
          BuildPlane(GImGuizmo.bounds.anchor, bestAxisWorldDirection);
        GImGuizmo.bounds.bestAxis = bestAxis;
        GImGuizmo.bounds.axis[0] = secondAxis;
        GImGuizmo.bounds.axis[1] = thirdAxis;

        GImGuizmo.bounds.localPivot = glm::vec3{ 0.0f };
        GImGuizmo.bounds.localPivot[secondAxis] =
          aabb[oppositeIndex][secondAxis];
        GImGuizmo.bounds.localPivot[thirdAxis] = aabb[oppositeIndex][thirdAxis];

        GImGuizmo.bounds.inUse = true;
        //GImGuizmo.gizmo.editingID = GImGuizmo.gizmo.actualID;
        GImGuizmo.bounds.matrix = GImGuizmo.modelSourceMatrix;
      }
      // Small anchor in the middle of the segment
      if (!GImGuizmo.bounds.inUse && GImGuizmo.enabled && overSmallAnchor &&
          CanActivate()) {
        const glm::vec3 midPointOpposite{
          (aabb[(i + 2) % 4] + aabb[(i + 3) % 4]) * 0.5f
        };
        GImGuizmo.bounds.pivot =
          GImGuizmo.modelSourceMatrix * glm::vec4{ midPointOpposite, 1.0f };
        GImGuizmo.bounds.anchor =
          GImGuizmo.modelSourceMatrix * glm::vec4{ midPoint, 1.0f };

        GImGuizmo.bounds.plane =
          BuildPlane(GImGuizmo.bounds.anchor, bestAxisWorldDirection);
        GImGuizmo.bounds.bestAxis = bestAxis;
        const int indices[]{ secondAxis, thirdAxis };
        GImGuizmo.bounds.axis[0] = indices[i % 2];
        GImGuizmo.bounds.axis[1] = -1;

        GImGuizmo.bounds.localPivot = glm::vec3{ 0.0f };
        GImGuizmo.bounds.localPivot[GImGuizmo.bounds.axis[0]] =
          aabb[oppositeIndex][indices[i % 2]];

        GImGuizmo.bounds.inUse = true;
        //GImGuizmo.gizmo.editingID = GImGuizmo.gizmo.actualID;
        GImGuizmo.bounds.matrix = GImGuizmo.modelSourceMatrix;
      }
    }

    if (GImGuizmo.bounds.inUse) {
      glm::mat4 scale{ 1.0f };

      // Compute projected mouse position on plane
      const float len{ IntersectRayPlane(GImGuizmo.ray, GImGuizmo.bounds.plane) };
      const glm::vec3 newPos{ GImGuizmo.ray.start +
                              GImGuizmo.ray.direction * len };

      // Compute a reference and delta vectors base on mouse move
      const glm::vec3 deltaVector{ glm::abs(newPos - GImGuizmo.bounds.pivot) };
      const glm::vec3 referenceVector{ glm::abs(GImGuizmo.bounds.anchor -
                                                GImGuizmo.bounds.pivot) };

      // For 1 or 2 axes, compute a ratio that's used for scale and snap it
      // based on resulting length
      for (int i = 0; i < 2; i++) {
        const int axisIndex1{ GImGuizmo.bounds.axis[i] };
        if (axisIndex1 == -1) continue;

        float ratioAxis{ 1.0f };
        const glm::vec3 axisDir{ glm::abs(GImGuizmo.bounds.matrix[axisIndex1]) };

        const float dtAxis{ glm::dot(axisDir, referenceVector) };
        const float boundSize{ bounds[axisIndex1 + 3] - bounds[axisIndex1] };
        if (dtAxis > FLT_EPSILON)
          ratioAxis = glm::dot(axisDir, deltaVector) / dtAxis;

        if (snapValues) {
          float length{ boundSize * ratioAxis };
          ComputeSnap(length, snapValues[axisIndex1]);
          if (boundSize > FLT_EPSILON) ratioAxis = length / boundSize;
        }
        scale[axisIndex1] *= ratioAxis;
      }

      // Transform matrix
      const glm::mat4 preScale{ glm::translate(-GImGuizmo.bounds.localPivot) };
      const glm::mat4 postScale{ glm::translate(GImGuizmo.bounds.localPivot) };
      matrix = GImGuizmo.bounds.matrix * postScale * scale * preScale;

      char scaleInfo[512]{};
      ImFormatString(
        scaleInfo, sizeof(scaleInfo), "X: %.2f Y: %.2f Z: %.2f",
        (bounds[3] - bounds[0]) * glm::length(GImGuizmo.bounds.matrix[0]) *
          glm::length(scale[0]),
        (bounds[4] - bounds[1]) * glm::length(GImGuizmo.bounds.matrix[1]) *
          glm::length(scale[1]),
        (bounds[5] - bounds[2]) * glm::length(GImGuizmo.bounds.matrix[2]) *
          glm::length(scale[2]));

      DrawText(GImGuizmo.gizmo.origin, scaleInfo);
    }

    if (!io.MouseDown[0]) {
      GImGuizmo.bounds.inUse = false;
      //GImGuizmo.gizmo.editingID = -1;
    }
    if (GImGuizmo.bounds.inUse) break;
  }
}

/**
 * @param [out] frustum
 * @param [in] clip
 */
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

}; // namespace priv

//-----------------------------------------------------------------------------
// [SECTION] PUBLIC INTERFACE
//-----------------------------------------------------------------------------

namespace ImGuizmo {

void PrintContext() {
  ImGui::Begin("ImGuizmo::Debug");

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  ImGui::Checkbox("InUse", &GImGuizmo.gizmo.inUse);
  ImGui::PopItemFlag();

  ImGui::Text("CurrentMoveType: %s",
              to_str(ImGuizmoMoveType_(GImGuizmo.gizmo.currentMoveType)));
  //ImGui::Text("Operation: %s", to_str(GImGuizmo.gizmo.operation));

  //ImGui::Text("ID, actual = %d, editing: %d", GImGuizmo.gizmo.actualID,
  //            GImGuizmo.gizmo.editingID);

  if (ImGui::TreeNode("Gizmo")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Text("origin (screen space): [%0.2f, %0.2f]",
                GImGuizmo.gizmo.origin.x, GImGuizmo.gizmo.origin.y);
    ImGui::Text("ringRadius: %.2f", GImGuizmo.gizmo.ringRadius);

    /*
    ImGui::Text("mScreenSquareMin: [%.2f, %.2f]", GImGuizmo.gizmo.aabb.Min.x,
                GImGuizmo.gizmo.aabb.Min.y);
    ImGui::Text("mScreenSquareMax: [%.2f, %.2f]", GImGuizmo.gizmo.aabb.Max.x,
                GImGuizmo.gizmo.aabb.Max.y);
                */

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Camera")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("Right", &GImGuizmo.camera.right[0], "%.2f");
    ImGui::InputFloat3("Up", &GImGuizmo.camera.up[0], "%.2f");
    ImGui::InputFloat3("Forward", &GImGuizmo.camera.forward[0], "%.2f");
    ImGui::InputFloat3("Eye", &GImGuizmo.camera.eye[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Ray")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("start", &GImGuizmo.ray.start[0], "%.2f");
    ImGui::InputFloat3("end", &GImGuizmo.ray.end[0], "%.2f");
    ImGui::InputFloat3("direction", &GImGuizmo.ray.direction[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Bounds")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Checkbox("InUse", &GImGuizmo.bounds.inUse);
    ImGui::InputFloat3("Pivot", &GImGuizmo.bounds.pivot[0]);
    ImGui::InputFloat3("Anchor", &GImGuizmo.bounds.anchor[0]);
    ImGui::InputFloat3("LocalPivot", &GImGuizmo.bounds.localPivot[0]);
    ImGui::InputFloat4("Plane", &GImGuizmo.bounds.plane[0]);
    ImGui::InputInt("BestAxis", &GImGuizmo.bounds.bestAxis);
    ImGui::InputInt2("Axis", &GImGuizmo.bounds.axis[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Translation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("dragTranslationOrigin",
                       &GImGuizmo.dragTranslationOrigin[0]);
    ImGui::InputFloat3("lastTranslationDelta",
                       &GImGuizmo.lastTranslationDelta[0]);
    ImGui::InputFloat3("relativeOrigin", &GImGuizmo.gizmo.modelRelativeOrigin[0]);
    ImGui::InputFloat3("translationPlaneOrigin",
                       &GImGuizmo.translationPlaneOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Rotation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("rotationVectorSource",
                       &GImGuizmo.rotationVectorSource[0]);
    ImGui::InputFloat("rotationAngle", &GImGuizmo.rotationAngle);
    ImGui::InputFloat("rotationAngleOrigin", &GImGuizmo.rotationAngleOrigin);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Scale")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("modelScaleOrigin", &GImGuizmo.gizmo.modelScaleOrigin[0]);
    ImGui::InputFloat3("scaleValueOrigin", &GImGuizmo.scaleValueOrigin[0]);
    ImGui::InputFloat3("scale", &GImGuizmo.scale[0]);
    ImGui::InputFloat3("lastScale", &GImGuizmo.lastScale[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  /*
  ImGui::Text("belowAxisLimit");
  for (int i = 0; i < 3; ++i) {
    ImGui::SameLine();
    ImGui::Checkbox("", &gContext.belowAxisLimit[i]);
  }
  ImGui::Text("belowPlaneLimit");
  for (int i = 0; i < 3; ++i) {
    ImGui::SameLine();
    ImGui::Checkbox("", &gContext.belowPlaneLimit[i]);
  }
  */
  //ImGui::InputFloat3("axisFactor", &gContext.gizmo.axisDirectionFactor[0]);
  ImGui::PopItemFlag();

  ImGui::End();
}

void Enable(bool enabled) {
  GImGuizmo.enabled = enabled;
  if (!enabled) {
    GImGuizmo.gizmo.inUse = false;
    GImGuizmo.bounds.inUse = false;
  }
}

void SetDrawlist(ImDrawList *drawList) {
  GImGuizmo.drawList = drawList ? drawList : ImGui::GetWindowDrawList();
}
void SetViewport(const ImRect &viewport) {
  GImGuizmo.viewport = viewport;
  GImGuizmo.aspectRatio = viewport.GetWidth() / viewport.GetHeight();
}
void SetViewport(const ImVec2 &position, const ImVec2 &size) {
  SetViewport({ position, size });
}
void SetViewport(float x, float y, float width, float height) {
  SetViewport({ x, y, width, height });
}

void SetupWorkspace(const char *name, const ImVec2 &position,
                    const ImVec2 &size) {
  ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
  ImGui::PushStyleColor(ImGuiCol_Border, 0);
  ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

  static const ImGuiWindowFlags flags{
    ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoResize |
    ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoInputs |
    ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing |
    ImGuiWindowFlags_NoBringToFrontOnFocus
  };
  ImGui::SetNextWindowPos(position);
  ImGui::SetNextWindowSize(size);
  ImGui::Begin(name, nullptr, flags);
  SetDrawlist(nullptr);
  SetViewport(position, size);
  ImGui::End();
  ImGui::PopStyleVar();
  ImGui::PopStyleColor(2);
}

void SetViewer(const float *view, const float *projection, bool isOrtho) {
  IM_ASSERT(view && projection);

  ImGuizmoContext &g{ GImGuizmo };

  g.camera.viewMatrix = glm::make_mat4(view);
  const glm::mat4 inversedViewMatrix{ glm::inverse(g.camera.viewMatrix) };
  g.camera.right = inversedViewMatrix[0];
  g.camera.up = inversedViewMatrix[1];
  g.camera.forward = inversedViewMatrix[2];
  g.camera.eye = inversedViewMatrix[3];

  g.camera.isOrtho = isOrtho;
  g.camera.projectionMatrix = glm::make_mat4(projection);

  g.camera.viewProjectionMatrix =
    g.camera.projectionMatrix * g.camera.viewMatrix;

  if (g.enabled) {
    g.ray =
      ScreenToWorldRay(ImGui::GetIO().MousePos, g.camera.viewProjectionMatrix,
                       g.viewport.GetTL(), g.viewport.GetBR());
  }
}

bool Manipulate(ImGuizmoMode_ mode, ImGuizmoOperationFlags flags, float *model,
                float *deltaMatrix, const float *snap) {
  IM_ASSERT(model);

  ImGuizmoContext &g{ GImGuizmo };
  g.gizmo.mode = mode;

  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::mat4{ 1.0f };

  glm::mat4 modelMatrix{ glm::make_mat4(model) };
  if (g.gizmo.mode == ImGuizmoMode_Local) {
    g.modelMatrix = modelMatrix;
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      g.modelMatrix[axis] = glm::normalize(g.modelMatrix[axis]);
  } else {
    g.modelMatrix = glm::translate(glm::vec3{ modelMatrix[3] });
  }
  g.modelViewProjMatrix = g.camera.viewProjectionMatrix * g.modelMatrix;

  g.modelSourceMatrix = modelMatrix;
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    g.gizmo.modelScaleOrigin[axis] = glm::length(g.modelSourceMatrix[axis]);

  // ---

  g.gizmo.origin = WorldToScreen(glm::vec3{ 0.0f }, g.modelViewProjMatrix);

  g.inversedModelMatrix = glm::inverse(g.modelMatrix);
  const glm::vec3 rightViewInverse{ g.inversedModelMatrix *
                                    glm::vec4{ g.camera.right, 0.0f } };
  const float rightLength{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                     rightViewInverse) };
  g.screenFactor = g.style.GizmoScale / rightLength;

  // ---

  const glm::vec3 camSpacePosition{ g.modelViewProjMatrix *
                                    glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  if (!g.camera.isOrtho && camSpacePosition.z < 0.001f) return false;

  // ---

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  bool manipulated{ false };
  if (g.enabled && !g.bounds.inUse) {
    if (flags & ImGuizmoOperationFlags_Translate && CanTranslate(moveType)) {
      manipulated |=
        priv::HandleTranslation(moveType, modelMatrix, deltaMatrix, snap);
    }
    if (flags & ImGuizmoOperationFlags_Rotate && CanRotate(moveType)) {
      manipulated |=
        priv::HandleRotation(moveType, modelMatrix, deltaMatrix, snap);
    }
    if (flags & ImGuizmoOperationFlags_Scale && CanScale(moveType)) {
      manipulated |=
        priv::HandleScale(moveType, modelMatrix, deltaMatrix, snap);
    }

    if (!ImGui::GetIO().MouseDown[0]) GImGuizmo.gizmo.inUse = false;
  }

/*
  if (localBounds && !GImGuizmo.gizmo.inUse) {
    priv::HandleAndDrawLocalBounds(localBounds, modelMatrix, boundsSnap,
                                   operation);
  }
  */

  *reinterpret_cast<glm::mat4 *>(model) = std::move(modelMatrix);

  //
  //
  //

  if (!g.bounds.inUse) {
    if (flags & ImGuizmoOperationFlags_Translate) {
      priv::DrawTranslationGizmo(
        IsTranslation(moveType) ? moveType : ImGuizmoMoveType_None);
    }
    if (flags & ImGuizmoOperationFlags_Rotate) {
      priv::DrawRotationGizmo(IsRotation(moveType) ? moveType
                                                   : ImGuizmoMoveType_None);
    }
    if (flags & ImGuizmoOperationFlags_Scale) {
      priv::DrawScaleGizmo(IsScale(moveType) ? moveType
                                             : ImGuizmoMoveType_None);
    }
  }

  return manipulated;
}

void ViewManipulate(float *view, const float length, ImVec2 position,
                    ImVec2 size, ImU32 backgroundColor) {
  const ImGuiIO &io{ ImGui::GetIO() };

  //ImDrawList *drawList{ gContext.drawList };
  ImDrawList *drawList{ ImGui::GetCurrentWindow()->DrawList };

  static bool isDragging{ false };
  static bool isClicking{ false };
  static bool isInside{ false };

  static glm::vec3 interpolationUp{ 0.0f };
  static glm::vec3 interpolationDir{ 0.0f };
  static int interpolationFrames{ 0 };

  drawList->AddRectFilled(position, position + size, backgroundColor);

  const glm::mat4 inversedViewMatrix{ glm::inverse(
    *reinterpret_cast<const glm::mat4 *>(view)) };

  constexpr float kDistance{ 2.0f };
  const glm::mat4 cubeProjection{ glm::perspective(
    glm::radians(60.0f), size.x / size.y, 0.01f, 1000.0f) };

  const glm::vec3 forward{ inversedViewMatrix[2][0], inversedViewMatrix[2][1],
                           inversedViewMatrix[2][2] };
  const glm::vec3 up{ inversedViewMatrix[1][0], inversedViewMatrix[1][1],
                      inversedViewMatrix[1][2] };
  const glm::vec3 eye{ forward * kDistance };
  const glm::mat4 cubeView{ glm::lookAt(eye, glm::vec3{ 0.0f }, up) };

  const ImGuizmoRay ray{ ScreenToWorldRay(
    io.MousePos, cubeProjection * cubeView, position, size) };

  static const glm::vec2 kPanelPosition[]{ { 0.75f, 0.75f }, { 0.25f, 0.75f },
                                           { 0.00f, 0.75f }, { 0.75f, 0.25f },
                                           { 0.25f, 0.25f }, { 0.00f, 0.25f },
                                           { 0.75f, 0.00f }, { 0.25f, 0.00f },
                                           { 0.00f, 0.00f } };
  static const glm::vec2 kPanelSize[]{ { 0.25f, 0.25f }, { 0.5f, 0.25f },
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
      const glm::vec3 indexVectorX{ kUnitDirection[perpXIndex] * invert };
      const glm::vec3 indexVectorY{ kUnitDirection[perpYIndex] * invert };
      const glm::vec3 boxOrigin{ kUnitDirection[normalIndex] * -invert -
                                 indexVectorX - indexVectorY };

      /*
      const glm::vec3 faceCoords[]{
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex]
      };
      */

      // Plane local space
      const glm::vec3 n{ kUnitDirection[normalIndex] * invert };
      const glm::vec3 viewSpaceNormal{ glm::vec3{
        glm::normalize(cubeView * glm::vec4{ n, 0.0f }) } };
      const glm::vec3 viewSpacePoint{ cubeView * glm::vec4{ n * 0.5f, 1.0f } };
      const glm::vec4 viewSpaceFacePlane{ BuildPlane(viewSpacePoint,
                                                           viewSpaceNormal) };

      if (viewSpaceFacePlane.w > 0) continue; // Back face culling

      const glm::vec4 facePlane{ BuildPlane(n * 0.5f, n) };
      const float len{ IntersectRayPlane(ray, facePlane) };
      const glm::vec3 posOnPlane{ ray.start + ray.direction * len -
                                  (n * 0.5f) };

      const float localX{
        glm::dot(kUnitDirection[perpXIndex], posOnPlane) * invert + 0.5f
      };
      const float localY{
        glm::dot(kUnitDirection[perpYIndex], posOnPlane) * invert + 0.5f
      };

      // Panels
      const glm::vec3 dx{ kUnitDirection[perpXIndex] };
      const glm::vec3 dy{ kUnitDirection[perpYIndex] };
      const glm::vec3 origin{ kUnitDirection[normalIndex] - dx - dy };
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
    if (dt < 0.0f) {
      newDir += planeDir * dt;
      newDir = glm::normalize(newDir);
    }
    const glm::vec3 newEye{ cameraTarget + newDir * length };
    *reinterpret_cast<glm::mat4 *>(view) =
      glm::lookAt(newEye, cameraTarget, kReferenceUp);
  }
}

bool IsUsing() { return GImGuizmo.gizmo.inUse || GImGuizmo.bounds.inUse; }
//bool IsOver() { return IsOver(GImGuizmo.gizmo.operation); }
/*
bool IsOver(ImGuizmoOperation_ operation) {
  const glm::vec2 mousePosition{ ImGui::GetIO().MousePos };
  switch (operation) {
  case ImGuizmoOperation_Translate:
    return priv::GetTranslateType(mousePosition) != ImGuizmoMoveType_None;
  case ImGuizmoOperation_Rotate:
    return priv::GetRotateType(mousePosition) != ImGuizmoMoveType_None;
  case ImGuizmoOperation_Scale:
    return priv::GetScaleType(mousePosition) != ImGuizmoMoveType_None;
  }

  return false;
}*/

void DrawCubes(const float *view, const float *projection,
               const float *modelMatrices, int modelMatrixCount) {
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
  priv::ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));
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
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex],
      };

      const glm::vec3 centerPosition{
        modelMatrix *
        glm::vec4{ (kUnitDirection[normalIndex] * 0.5f * invert), 1.0f }
      };
      const glm::vec4 centerPositionVP{
        modelViewProjMatrix *
        glm::vec4{ (kUnitDirection[normalIndex] * 0.5f * invert), 1.0f }
      };

      bool inFrustum{ true };
      for (int iFrustum = 0; iFrustum < 6; ++iFrustum) {
        const float dist{ DistanceToPlane(centerPosition,
                                                frustum[iFrustum]) };
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

      cubeFace.color =
        GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0x808080;
      // cubeFace.color = priv::GetColorU32(ImGuizmoCol_AxisX + normalIndex,
      // 0.9);//      | 0x808080;

      cubeFace.z = centerPositionVP.z / centerPositionVP.w;
      cubeFaceCount++;
    }
  }

  qsort(faces.begin(), cubeFaceCount, sizeof(cubeFace_t),
        [](void const *_a, void const *_b) {
          cubeFace_t *a = (cubeFace_t *)_a;
          cubeFace_t *b = (cubeFace_t *)_b;
          if (a->z < b->z) {
            return 1;
          }
          return -1;
        });

  for (int iFace = 0; iFace < cubeFaceCount; ++iFace) {
    const cubeFace_t &cubeFace{ faces[iFace] };
    GImGuizmo.drawList->AddConvexPolyFilled(cubeFace.faceCoordsScreen, 4,
                                           cubeFace.color);
  }
}
void DrawGrid(const float *view, const float *projection, const float *model,
              const float gridSize) {
  const glm::mat4 viewProjectionMatrix{ glm::make_mat4(projection) *
                                        glm::make_mat4(view) };

  glm::vec4 frustum[6]{};
  priv::ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));

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

        col = (fmodf(fabsf(f), 10.0f) < FLT_EPSILON) ? 0xFF909090 : col;
        col = (fabsf(f) < FLT_EPSILON) ? kDarkGrayColor : col;

        float thickness{ 1.0f };
        thickness = (fmodf(fabsf(f), 10.0f) < FLT_EPSILON) ? 1.5f : thickness;
        thickness = (fabsf(f) < FLT_EPSILON) ? 2.3f : thickness;

        GImGuizmo.drawList->AddLine(
          WorldToScreen(ptA, modelViewProjMatrix),
          WorldToScreen(ptB, modelViewProjMatrix), col, thickness);
      }
    }
  }
}

void DecomposeMatrix(const float *matrix, float *translation, float *rotation,
                     float *scale) {
  IM_ASSERT(matrix);

  auto mat = *reinterpret_cast<const glm::mat4 *>(matrix);
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    if (scale) scale[axis] = glm::length(mat[axis]);
    mat[axis] = glm::normalize(mat[axis]);
  }
  if (rotation) {
    rotation[0] = glm::degrees(glm::atan(mat[1][2], mat[2][2]));
    rotation[1] = glm::degrees(glm::atan(
      -mat[0][2], glm::sqrt(mat[1][2] * mat[1][2] + mat[2][2] * mat[2][2])));
    rotation[2] = glm::degrees(glm::atan(mat[0][1], mat[0][0]));
  }
  if (translation) {
    for (int i = 0; i < 3; ++i)
      translation[i] = mat[3][i];
  }
}
void RecomposeMatrix(const float *translation, const float *rotation,
                     const float *scale, float *matrix) {
  IM_ASSERT(matrix && translation && rotation && scale);

  glm::mat4 mat{ 1.0f };
  for (int i = 2; i >= 0; --i)
    mat *= glm::rotate(glm::radians(rotation[i]), kUnitDirection[i]);

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const auto validScale =
      glm::abs(scale[axis]) < FLT_EPSILON ? 0.001f : scale[axis];
    mat[axis] *= validScale;
  }
  mat[3] = glm::vec4{ glm::make_vec3(translation), 1.0f };
  *reinterpret_cast<glm::mat4 *>(matrix) = std::move(mat);
}

}; // namespace ImGuizmo

bool ImGuizmoContext::IsInsideViewport(const glm::vec2 &point) const {
  return viewport.Contains(point);
}
bool ImGuizmoContext::OverGizmoOrigin(const glm::vec2 &mousePosition) const {
  constexpr float kMargin{ 10.0f };
  ImRect aabb{ gizmo.origin - kMargin, gizmo.origin + kMargin };
  return aabb.Contains(mousePosition);
}
bool ImGuizmoContext::OverCameraAlignedRing(
  const glm::vec2 &mousePosition) const {
  constexpr float kTolerance{ 1.0f };
  const glm::vec2 deltaScreen{ mousePosition - gizmo.origin };
  const float dist{ glm::length(deltaScreen) };
  return (
    dist >= (gizmo.ringRadius - (style.RotationRingThickness + kTolerance)) &&
    dist < (gizmo.ringRadius + (style.RotationRingThickness + kTolerance)));
}
