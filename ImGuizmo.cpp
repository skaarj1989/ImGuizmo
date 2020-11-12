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

// TODO:
// - Move following methods to ImGuizmoWidget:
//    'Get<Move>Type'
//    'Begin<Move>'
//    'Continue<Move>'
// - Refactor:
//    'ComputeColors'
//    'ViewManipulate'
//    'HandleAndDrawLocalBounds'
// - Remove 'Manipulate'

//-----------------------------------------------------------------------------
// [SECTION] CONSTANTS
//-----------------------------------------------------------------------------

constexpr auto kPi = glm::pi<float>();
constexpr auto kEpsilon = glm::epsilon<float>();

const glm::vec3 kReferenceUp{ 0.0f, 1.0f, 0.0f };
static const glm::vec3 kUnitDirections[3]{
  { 1.0f, 0.0f, 0.0f }, // right
  { 0.0f, 1.0f, 0.0f }, // up
  { 0.0f, 0.0f, 1.0f }  // forward
};

constexpr float kQuadMin{ 0.5f };
constexpr float kQuadMax{ 0.8f };
static const float kQuadUV[]{ kQuadMin, kQuadMin, kQuadMin, kQuadMax,
                              kQuadMax, kQuadMax, kQuadMax, kQuadMin };

//-----------------------------------------------------------------------------
// [SECTION] TYPES
//-----------------------------------------------------------------------------

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
enum ImGuizmoOperation_ {
  ImGuizmoOperation_Translate,
  ImGuizmoOperation_Rotate,
  ImGuizmoOperation_Scale,

  ImGuizmoOperation_Bounds,

  ImGuizmoOperation_COUNT
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

using ImGuizmoMode = int;
using ImGuizmoAxis = int;
using ImGuizmoPlane = int;
using ImGuizmoMoveType = int;

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
  bool InUse{ false };
  ImGuizmoMoveType CurrentMoveType{ ImGuizmoMoveType_None };

  glm::mat4 ModelMatrix{ 1.0f };
  glm::mat4 ModelViewProjMatrix{ 1.0f };

  glm::mat4 ModelSourceMatrix{ 1.0f };
  glm::mat4 InversedModelMatrix{ 1.0f };

  glm::vec2 Origin{ 0.0f }; // In screen space
  float RingRadius{ 0.0f };

  glm::vec3 ModelScaleOrigin{ 1.0f };
  glm::vec3 ModelRelativeOrigin{ 0.0f };

  // Translation
  glm::vec4 TranslationPlane{ 0.0f };
  glm::vec3 TranslationPlaneOrigin{ 0.0f };
  glm::vec3 DragTranslationOrigin{ 0.0f };
  glm::vec3 LastTranslationDelta{ 0.0f };

  // Rotation
  glm::vec3 RotationVectorSource{ 0.0f };
  float RotationAngle{ 0.0f }; // In radians
  float RotationAngleOrigin{ 0.0f }; // In radians

  // Scale
  glm::vec3 Scale{ 1.0f };
  glm::vec3 ScaleValueOrigin{ 1.0f };
  glm::vec3 LastScale{ 1.0f };

  // ---

  bool IsMouseOverOrigin() const;
  bool IsMouseOverRotationRing() const;
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

constexpr float kCircleRadius{ 6.0f };  // Translation and scale dots
constexpr float kLineThickness{ 3.0f }; // Translation and scale axes

struct ImGuizmoContext {
  bool Enabled{ true };
  ImGuizmoMode_ Mode{ ImGuizmoMode_Local };
  ImDrawList *DrawList{ nullptr };

  ImGuizmoStyle Style;

  ImRect Viewport;
  ImGuizmoCamera Camera;
  ImGuizmoRay Ray;
  glm::vec2 DragOrigin{ 0.0f };

  float ScreenFactor{ 0.0f };
  ImGuizmoWidget Gizmo;
  ImGuizmoCage Bounds;

  bool Manipulated{ false };
  float *LockedModelMatrix{ nullptr };

  // ---

  float GetAspectRatio() const;
};
static ImGuizmoContext GImGuizmo;

//-----------------------------------------------------------------------------
// [SECTION] STYLING
//-----------------------------------------------------------------------------

ImGuizmoStyle::ImGuizmoStyle() { ImGuizmo::StyleColorsDefault(this); }

namespace ImGuizmo {

ImGuizmoStyle &GetStyle() { return GImGuizmo.Style; }
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

static ImU32 GetColorU32(ImGuiCol idx, float alpha_mul = 1.0f) {
  const ImGuizmoStyle &style{ GImGuizmo.Style };
  ImVec4 c{ style.Colors[idx] };
  c.w *= style.Alpha * alpha_mul;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static ImU32 GetColorU32(const ImVec4 &col) {
  const ImGuizmoStyle &style{ GImGuizmo.Style };
  ImVec4 c{ col };
  c.w *= style.Alpha;
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

  glm::vec2 screenPosition{ glm::vec2{ temp } + 0.5f };
  screenPosition.y = 1.0f - screenPosition.y;
  screenPosition *= size;
  screenPosition += position;
  return screenPosition;
}
static glm::vec2 WorldToScreen(const glm::vec3 &worldPos,
                               const glm::mat4 &matrix) {
  return WorldToScreen(worldPos, matrix, GImGuizmo.Viewport.GetTL(),
                       GImGuizmo.Viewport.GetBR());
}

static ImGuizmoRay RayCast(const glm::mat4 &viewProjMatrix,
                           const glm::vec2 &position, const glm::vec2 &size) {
  const glm::vec2 &mousePosition{ ImGui::GetIO().MousePos };
  const float x{ ((mousePosition.x - position.x) / size.x) * 2.0f - 1.0f };
  const float y{ (1.0f - ((mousePosition.y - position.y) / size.y)) * 2.0f -
                 1.0f };

  const glm::mat4 inversedViewProj{ glm::inverse(viewProjMatrix) };
  glm::vec4 rayStartWorldSpace{ inversedViewProj *
                                glm::vec4{ x, y, 0.0f, 1.0f } };
  rayStartWorldSpace *= 1.0f / rayStartWorldSpace.w;
  glm::vec4 rayEndWorldSpace{ inversedViewProj *
                              glm::vec4{ x, y, 1.0f - kEpsilon, 1.0f } };
  rayEndWorldSpace *= 1.0f / rayEndWorldSpace.w;
  return ImGuizmoRay{ rayStartWorldSpace, rayEndWorldSpace,
                      glm::normalize(rayEndWorldSpace - rayStartWorldSpace) };
}

static float GetParallelogram(const glm::vec4 &ptO, const glm::vec4 &ptA,
                              const glm::vec4 &ptB) {
  const ImGuizmoContext &g{ GImGuizmo };

  glm::vec4 points[]{ ptO, ptA, ptB };
  for (int i = 0; i < 3; ++i) {
    points[i].w = 1.0f;
    points[i] = g.Gizmo.ModelViewProjMatrix * points[i];
    // Check for axis aligned with camera direction
    if (glm::abs(points[i].w) > kEpsilon) {
      points[i] *= 1.0f / points[i].w;
    }
  }
  glm::vec4 segA{ points[1] - points[0] };
  segA.y /= g.GetAspectRatio();
  glm::vec4 segB{ points[2] - points[0] };
  segB.y /= g.GetAspectRatio();

  const auto segAOrtho =
    glm::normalize(glm::vec4{ -segA.y, segA.x, 0.0f, 0.0f });
  const float dt{ glm::dot(segAOrtho, segB) };
  const float surface{ glm::length(segA) * glm::abs(dt) };
  return surface;
}

static glm::vec4 BuildPlane(const glm::vec3 &point, const glm::vec3 &normal) {
  const auto n = glm::normalize(normal);
  return glm::vec4{ n, glm::dot(n, point) };
}
static float DistanceToPlane(const glm::vec3 &point, const glm::vec4 &plane) {
  return glm::dot(glm::vec3{ plane }, point) + plane.w;
}
static float IntersectRayPlane(const ImGuizmoRay &ray, const glm::vec4 &plane) {
  const float num{ glm::dot(glm::vec3{ plane }, ray.Start) - plane.w };
  const float denom{ glm::dot(glm::vec3{ plane }, ray.Direction) };

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

  auto startOfSegment = g.Gizmo.ModelViewProjMatrix * glm::vec4{ start, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(startOfSegment.w) > kEpsilon)
    startOfSegment *= 1.0f / startOfSegment.w;

  auto endOfSegment = g.Gizmo.ModelViewProjMatrix * glm::vec4{ end, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(endOfSegment.w) > kEpsilon)
    endOfSegment *= 1.0f / endOfSegment.w;

  glm::vec2 clipSpaceAxis{ endOfSegment - startOfSegment };
  clipSpaceAxis.y /= g.GetAspectRatio();
  return glm::length(clipSpaceAxis);
}

//-----------------------------------------------------------------------------
// [SECTION]
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
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    ComputeSnap(value[axis], snap[axis]);
}

//-----------------------------------------------------------------------------
// [SECTION]
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
  return (moveType >= ImGuizmoMoveType_MoveX &&
          moveType <= ImGuizmoMoveType_MoveScreen);
}
static bool IsRotation(ImGuizmoMoveType moveType) {
  return (moveType >= ImGuizmoMoveType_RotateX &&
          moveType <= ImGuizmoMoveType_RotateScreen);
}
static bool IsScale(ImGuizmoMoveType moveType) {
  return (moveType >= ImGuizmoMoveType_ScaleX &&
          moveType <= ImGuizmoMoveType_ScaleXYZ);
}

static bool CanTranslate(ImGuizmoMoveType moveType) {
  return (moveType == ImGuizmoMoveType_None || IsTranslation(moveType));
}
static bool CanRotate(ImGuizmoMoveType moveType) {
  return (moveType == ImGuizmoMoveType_None || IsRotation(moveType));
}
static bool CanScale(ImGuizmoMoveType moveType) {
  return (moveType == ImGuizmoMoveType_None || IsScale(moveType));
}

static bool CanActivate() {
  if (ImGui::IsMouseClicked(0) && !ImGui::IsAnyItemHovered() &&
      !ImGui::IsAnyItemActive()) {
    return true;
  }
  return false;
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

static const int kTranslationInfoIndex[]{ 0, 0, 0, 1, 0, 0, 2, 0, 0, 1, 2,
                                          0, 0, 2, 0, 0, 1, 0, 0, 1, 2 };

static void DrawText(const glm::vec2 &position, const char *text) {
  const ImGuizmoContext &g{ GImGuizmo };
  g.DrawList->AddText(position + 15.0f, GetColorU32(ImGuizmoCol_TextShadow),
                      text);
  g.DrawList->AddText(position + 14.0f, GetColorU32(ImGuizmoCol_Text), text);
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
// [SECTION]
//-----------------------------------------------------------------------------

constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES
//-----------------------------------------------------------------------------

static void ComputeColors(ImU32 colors[7], ImGuizmoMoveType moveType,
                          ImGuizmoOperation_ operation) {
  if (!GImGuizmo.Enabled) {
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

static bool IsAxisVisible(const glm::vec3 &dirAxis) {
  constexpr float kVisibilityThreshold{ 0.02f };
  const float axisLength{ GetSegmentLengthClipSpace(
    glm::vec3{ 0.0f }, dirAxis * GImGuizmo.ScreenFactor) };
  return axisLength > kVisibilityThreshold;
}
static bool IsPlaneVisible(const glm::vec3 &dir1, const glm::vec3 &dir2) {
  constexpr float kVisibilityThreshold{ 0.0025f };
  const float surface{ GetParallelogram(
    glm::vec4{ 0.0f }, glm::vec4{ dir1, 0.0f } * GImGuizmo.ScreenFactor,
    glm::vec4{ dir2, 0.0f } * GImGuizmo.ScreenFactor) };
  return surface > kVisibilityThreshold;
}

static bool IsAxisVisible(ImGuizmoAxis axis) {
  return IsAxisVisible(kUnitDirections[axis]);
}
static bool IsPlaneVisible(ImGuizmoPlane plane) {
  return IsPlaneVisible(kUnitDirections[(plane + 1) % 3],
                        kUnitDirections[(plane + 2) % 3]);
}

static bool IsMouseOverAxis() {
  const ImGuizmoContext &g{ GImGuizmo };

  /*
  const glm::vec3 &modelPosition{ g.modelMatrix[3] };
  const float length{ IntersectRayPlane(g.ray,
                                        BuildPlane(modelPosition, dirAxis)) };
  const glm::vec3 mousePosOnPlane{ g.ray.start + g.ray.direction * length };
  */
  return true;
}

//-----------------------------------------------------------------------------
// [SECTION] TRANSLATION
//-----------------------------------------------------------------------------

static ImGuizmoMoveType GetTranslateType() {
  const ImGuizmoContext &g{ GImGuizmo };

  if (g.Gizmo.IsMouseOverOrigin()) return ImGuizmoMoveType_MoveScreen;

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
    const glm::vec3 dirAxis{ g.Gizmo.ModelMatrix *
                             glm::vec4{ kUnitDirections[axis], 0.0f } };
    const float length{ IntersectRayPlane(g.Ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 mousePosOnPlane{ g.Ray.Start + g.Ray.Direction * length };

    const glm::vec2 mousePosOnPlaneInSS{ WorldToScreen(
      mousePosOnPlane, g.Camera.ViewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.ScreenFactor * 0.1f,
      g.Camera.ViewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.ScreenFactor,
      g.Camera.ViewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      mousePosOnPlaneInSS, axisStartOnScreen, axisEndOnScreen) };
    constexpr float kTolerance{ 12.0f };
    if (glm::length(closestPointOnAxis - mousePosOnPlaneInSS) < kTolerance)
      moveType = ImGuizmoMoveType_MoveX + axis;

    // ---

    const glm::vec3 planeDir1{
      g.Gizmo.ModelMatrix * glm::vec4{ kUnitDirections[(axis + 1) % 3], 0.0f }
    };
    const float dx{ glm::dot(planeDir1, (mousePosOnPlane - modelPosition) *
                                          (1.0f / g.ScreenFactor)) };
    const glm::vec3 planeDir2{
      g.Gizmo.ModelMatrix * glm::vec4{ kUnitDirections[(axis + 2) % 3], 0.0f }
    };
    const float dy{ glm::dot(planeDir2, (mousePosOnPlane - modelPosition) *
                                          (1.0f / g.ScreenFactor)) };
    if (IsPlaneVisible(planeDir1, planeDir2) && dx >= kQuadUV[0] &&
        dx <= kQuadUV[4] && dy >= kQuadUV[1] && dy <= kQuadUV[3]) {
      moveType = ImGuizmoMoveType_MoveYZ + axis;
    }

    // ---

    if (moveType != ImGuizmoMoveType_None) break;
  }

  return moveType;
}

static ImGuizmoMoveType BeginTranslation() {
  ImGuizmoContext &g{ GImGuizmo };

  ImGuizmoMoveType moveType{ GetTranslateType() };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.Gizmo.InUse = true;
    g.Gizmo.CurrentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
    const glm::vec3 cameraToModelNormalized{ glm::normalize(modelPosition -
                                                            g.Camera.Eye) };
    glm::vec3 movePlaneNormal[]{ g.Gizmo.ModelMatrix[0], g.Gizmo.ModelMatrix[1],
                                 g.Gizmo.ModelMatrix[2], g.Gizmo.ModelMatrix[0],
                                 g.Gizmo.ModelMatrix[1], g.Gizmo.ModelMatrix[2],
                                 -g.Camera.Forward };

    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
      const glm::vec3 orthoVector{ glm::cross(movePlaneNormal[axis],
                                              cameraToModelNormalized) };
      movePlaneNormal[axis] =
        glm::normalize(glm::cross(movePlaneNormal[axis], orthoVector));
    }

    g.Gizmo.TranslationPlane = BuildPlane(
      modelPosition, movePlaneNormal[moveType - ImGuizmoMoveType_MoveX]);

    const auto length{ IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane) };
    g.Gizmo.TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
    g.Gizmo.DragTranslationOrigin = modelPosition;
    g.Gizmo.ModelRelativeOrigin =
      (g.Gizmo.TranslationPlaneOrigin - modelPosition) *
      (1.0f / g.ScreenFactor);
    g.DragOrigin = ImGui::GetIO().MousePos;
  }

  return moveType;
}
static bool ContinueTranslation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                                float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();

  const float length{ glm::abs(
    IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane)) };
  const glm::vec3 newPos{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 newOrigin{ newPos -
                             g.Gizmo.ModelRelativeOrigin * g.ScreenFactor };
  const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
  glm::vec3 delta{ newOrigin - modelPosition };

  // 1 axis constraint
  if (g.Gizmo.CurrentMoveType >= ImGuizmoMoveType_MoveX &&
      g.Gizmo.CurrentMoveType <= ImGuizmoMoveType_MoveZ) {
    const int axisIndex{ g.Gizmo.CurrentMoveType - ImGuizmoMoveType_MoveX };
    const glm::vec3 axisValue{ g.Gizmo.ModelMatrix[axisIndex] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
  }

  if (snap) {
    glm::vec3 cumulativeDelta{ modelPosition + delta -
                               g.Gizmo.DragTranslationOrigin };
    bool applyRotationLocaly{ g.Mode == ImGuizmoMode_Local ||
                              moveType == ImGuizmoMoveType_MoveScreen };
    if (applyRotationLocaly) {
      glm::mat4 modelSourceNormalized{ g.Gizmo.ModelSourceMatrix };
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
    delta = g.Gizmo.DragTranslationOrigin + cumulativeDelta - modelPosition;
  }

  bool modified{ false };
  if (delta != g.Gizmo.LastTranslationDelta) modified = true;
  g.Gizmo.LastTranslationDelta = delta;

  const glm::mat4 deltaMatrixTranslation{ glm::translate(delta) };
  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = deltaMatrixTranslation;

  moveType = g.Gizmo.CurrentMoveType;
  matrix = deltaMatrixTranslation * g.Gizmo.ModelSourceMatrix;

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

  if (!g.Gizmo.InUse) moveType = BeginTranslation();
  if (g.Gizmo.InUse)
    return ContinueTranslation(moveType, matrix, deltaMatrix, snap);

  return false;
}

static void DrawTranslationGizmo(ImGuizmoMoveType moveType) {
  const ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Translate);

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 &dirAxis{ kUnitDirections[axis] };
    if (IsAxisVisible(dirAxis)) {
      const glm::vec2 tail{ WorldToScreen(dirAxis * 0.1f * g.ScreenFactor,
                                          g.Gizmo.ModelViewProjMatrix) };
      const glm::vec2 head{ WorldToScreen(dirAxis * g.ScreenFactor,
                                          g.Gizmo.ModelViewProjMatrix) };

      g.DrawList->AddLine(tail, head, colors[axis + 1], kLineThickness);
      constexpr float kArrowheadSize{ kLineThickness * 2.0f };
      const glm::vec2 dir{ glm::normalize(g.Gizmo.Origin - head) *
                           kArrowheadSize };
      const glm::vec2 ortogonalDir{ dir.y, -dir.x };
      const glm::vec2 a{ head + dir };
      g.DrawList->AddTriangleFilled(head - dir, a + ortogonalDir,
                                    a - ortogonalDir, colors[axis + 1]);
    }

    const glm::vec3 &planeDir1{ kUnitDirections[(axis + 1) % 3] };
    const glm::vec3 &planeDir2{ kUnitDirections[(axis + 2) % 3] };
    if (IsPlaneVisible(planeDir1, planeDir2)) {
      ImVec2 screenQuadPts[4]{};
      for (int i = 0; i < 4; ++i) {
        const glm::vec3 cornerWorldSpace{ (planeDir1 * kQuadUV[i * 2] +
                                           planeDir2 * kQuadUV[i * 2 + 1]) *
                                          g.ScreenFactor };
        screenQuadPts[i] =
          WorldToScreen(cornerWorldSpace, g.Gizmo.ModelViewProjMatrix);
      }

      g.DrawList->AddConvexPolyFilled(screenQuadPts, 4, colors[axis + 4]);
      constexpr float kQuadBorder{ 1.5f };
      g.DrawList->AddPolyline(screenQuadPts, 4,
                              GetColorU32(ImGuizmoCol_AxisX + axis), true,
                              kQuadBorder);
    }
  }

  if (moveType != ImGuizmoMoveType_None && g.Gizmo.InUse) {
    const glm::vec2 tail{ WorldToScreen(g.Gizmo.DragTranslationOrigin,
                                        g.Camera.ViewProjectionMatrix) };
    const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
    const glm::vec2 head{ WorldToScreen(modelPosition,
                                        g.Camera.ViewProjectionMatrix) };
    const glm::vec2 diff{ glm::normalize(head - tail) *
                          (kCircleRadius - 1.0f) };

    constexpr auto kTranslationLineColor = 0xAAAAAAAA;
    constexpr float kMargin{ 1.5f };
    g.DrawList->AddCircle(tail, kCircleRadius + kMargin, kTranslationLineColor);
    g.DrawList->AddCircle(head, kCircleRadius + kMargin, kTranslationLineColor);
    g.DrawList->AddLine(tail + diff, head - diff, kTranslationLineColor, 2.0f);

    const glm::vec3 deltaInfo{ modelPosition - g.Gizmo.DragTranslationOrigin };
    DrawTranslationInfo(head, moveType, deltaInfo);
  }

  // *  g.screenFactor
  g.DrawList->AddCircleFilled(g.Gizmo.Origin, kCircleRadius, colors[0], 32);
}

//-----------------------------------------------------------------------------
// [SECTION] ROTATION
//-----------------------------------------------------------------------------

static float ComputeAngleOnPlane() {
  const ImGuizmoContext &g{ GImGuizmo };

  const float length{ IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane) };
  const glm::vec3 localPos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length -
    glm::vec3{ g.Gizmo.ModelMatrix[3] }) };

  const glm::vec3 perpendicularVec{ glm::normalize(glm::cross(
    g.Gizmo.RotationVectorSource, glm::vec3{ g.Gizmo.TranslationPlane })) };

  const float acosAngle{ glm::clamp(
    glm::dot(localPos, g.Gizmo.RotationVectorSource), -1.0f, 1.0f) };
  float angle{ glm::acos(acosAngle) };
  angle *= (glm::dot(localPos, perpendicularVec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}
static bool IsMouseOverRotationAxis(ImGuizmoAxis axis) {
  const ImGuizmoContext &g{ GImGuizmo };

  const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
  const glm::vec4 pickupPlane{ BuildPlane(modelPosition,
                                          g.Gizmo.ModelMatrix[axis]) };
  const float length{ IntersectRayPlane(g.Ray, pickupPlane) };
  const glm::vec3 localPos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - modelPosition) };

  // ... ?
  if (glm::dot(localPos, g.Ray.Direction) > kEpsilon) return false;

  const glm::vec3 idealPosOnCircle{ glm::mat3{ g.Gizmo.InversedModelMatrix } *
                                    localPos };
  const glm::vec2 idealPosOnCircleScreen{ WorldToScreen(
    idealPosOnCircle * g.ScreenFactor, g.Gizmo.ModelViewProjMatrix) };

  constexpr float kTolerance{ 8.0f };
  const glm::vec2 distanceOnScreen{ idealPosOnCircleScreen -
                                    ImGui::GetIO().MousePos };
  return glm::length(distanceOnScreen) < kTolerance;
}

static ImGuizmoMoveType GetRotateType() {
  const ImGuizmoContext &g{ GImGuizmo };
  if (g.Gizmo.IsMouseOverRotationRing()) return ImGuizmoMoveType_RotateScreen;
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    if (IsMouseOverRotationAxis(axis)) return ImGuizmoMoveType_RotateX + axis;

  return ImGuizmoMoveType_None;
}

static ImGuizmoMoveType BeginRotation() {
  ImGuizmoContext &g{ GImGuizmo };

  ImGuizmoMoveType moveType{ GetRotateType() };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();

  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.Gizmo.InUse = true;
    g.Gizmo.CurrentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
    const glm::vec3 rotatePlaneNormal[]{ g.Gizmo.ModelMatrix[0],
                                         g.Gizmo.ModelMatrix[1],
                                         g.Gizmo.ModelMatrix[2],
                                         -g.Camera.Forward };

    const bool applyRotationLocaly{ g.Mode == ImGuizmoMode_Local ||
                                    moveType == ImGuizmoMoveType_RotateScreen };
    if (applyRotationLocaly) {
      g.Gizmo.TranslationPlane = BuildPlane(
        modelPosition, rotatePlaneNormal[moveType - ImGuizmoMoveType_RotateX]);
    } else {
      g.Gizmo.TranslationPlane =
        BuildPlane(g.Gizmo.ModelSourceMatrix[3],
                   kUnitDirections[moveType - ImGuizmoMoveType_RotateX]);
    }

    const float length{ IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane) };
    const glm::vec3 localPos{ g.Ray.Start + g.Ray.Direction * length -
                              modelPosition };
    g.Gizmo.RotationVectorSource = glm::normalize(localPos);
    g.Gizmo.RotationAngleOrigin = ComputeAngleOnPlane();
    g.DragOrigin = ImGui::GetIO().MousePos;
  }

  return moveType;
}
static bool ContinueRotation(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                             float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();
  g.Gizmo.RotationAngle = ComputeAngleOnPlane();

  if (snap) ComputeSnap(g.Gizmo.RotationAngle, glm::radians(snap[0]));

  const glm::vec3 rotationAxisLocalSpace{ glm::normalize(
    glm::mat3{ g.Gizmo.InversedModelMatrix } *
    glm::vec3{ g.Gizmo.TranslationPlane }) };
  const glm::mat4 deltaRotation{ glm::rotate(g.Gizmo.RotationAngle -
                                               g.Gizmo.RotationAngleOrigin,
                                             rotationAxisLocalSpace) };

  const bool modified{ g.Gizmo.RotationAngle != g.Gizmo.RotationAngleOrigin };
  g.Gizmo.RotationAngleOrigin = g.Gizmo.RotationAngle;

  if (g.Mode == ImGuizmoMode_Local) {
    const glm::mat4 scaleOrigin{ glm::scale(g.Gizmo.ModelScaleOrigin) };
    matrix = g.Gizmo.ModelMatrix * deltaRotation * scaleOrigin;
  } else {
    glm::mat4 result{ g.Gizmo.ModelSourceMatrix };
    result[3] = glm::vec4{ glm::vec3{ 0.0f }, 1.0f };
    matrix = deltaRotation * result;
    matrix[3] = g.Gizmo.ModelSourceMatrix[3];
  }

  if (deltaMatrix) {
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) =
      g.Gizmo.ModelMatrix * deltaRotation * g.Gizmo.InversedModelMatrix;
  }

  moveType = g.Gizmo.CurrentMoveType;
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

  if (!g.Gizmo.InUse) moveType = BeginRotation();
  if (g.Gizmo.InUse)
    return ContinueRotation(moveType, matrix, deltaMatrix, snap);

  return false;
}

static void DrawRotationGizmo(ImGuizmoMoveType moveType) {
  ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Rotate);

  const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
  glm::vec3 cameraToModelNormalized{
    g.Camera.IsOrtho ? glm::inverse(g.Camera.ViewMatrix)[2]
                     : glm::normalize(modelPosition - g.Camera.Eye)
  };
  // Always face to camera
  cameraToModelNormalized =
    g.Gizmo.InversedModelMatrix * glm::vec4{ cameraToModelNormalized, 0.0f };

  constexpr float kRotationRingSize{ 0.06f };
  g.Gizmo.RingRadius = kRotationRingSize * g.Viewport.GetHeight();

  constexpr int kArcSegmentCount{ 64 };
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const float angleStart{ (glm::atan(
                              cameraToModelNormalized[(4 - axis) % 3],
                              cameraToModelNormalized[(3 - axis) % 3])) +
                            kPi * 0.5f };

    ImVec2 circlePos[kArcSegmentCount]{};
    for (int i = 0; i < kArcSegmentCount; ++i) {
      const float ng{ angleStart +
                      kPi * (static_cast<float>(i) / kArcSegmentCount) };
      const glm::vec3 axisPos{ glm::cos(ng), glm::sin(ng), 0.0f };
      const auto pos = glm::vec3{ axisPos[axis], axisPos[(axis + 1) % 3],
                                  axisPos[(axis + 2) % 3] } *
                       g.ScreenFactor;
      circlePos[i] = WorldToScreen(pos, g.Gizmo.ModelViewProjMatrix);
    }

    const float radiusAxis{ glm::length(g.Gizmo.Origin -
                                        glm::vec2{ circlePos[0] }) };
    if (radiusAxis > g.Gizmo.RingRadius) g.Gizmo.RingRadius = radiusAxis;
    // g.drawList->AddCircleFilled(circlePos[0], 6.5f,
    //                            colors[ImGuizmoAxis_COUNT - axis], 32);
    g.DrawList->AddPolyline(circlePos, kArcSegmentCount,
                            colors[ImGuizmoAxis_COUNT - axis], false, 2.5f);
  }

  // Circle parallel to view
  g.DrawList->AddCircle(g.Gizmo.Origin, g.Gizmo.RingRadius, colors[0],
                        kArcSegmentCount, g.Style.RotationRingThickness);

  if (moveType != ImGuizmoMoveType_None && g.Gizmo.InUse) {
    ImVec2 circlePos[kArcSegmentCount + 1]{ g.Gizmo.Origin };
    for (int i = 1; i < kArcSegmentCount; ++i) {
      const float ng{ g.Gizmo.RotationAngle *
                      (static_cast<float>(i - 1) / (kArcSegmentCount - 1)) };
      const glm::mat3 rotateVectorMatrix{ glm::rotate(
        ng, glm::vec3{ g.Gizmo.TranslationPlane }) };
      glm::vec3 pos{ rotateVectorMatrix * g.Gizmo.RotationVectorSource };
      pos *= g.ScreenFactor;
      circlePos[i] =
        WorldToScreen(pos + modelPosition, g.Camera.ViewProjectionMatrix);
    }

    // Draw inner part ...
    g.DrawList->AddConvexPolyFilled(circlePos, kArcSegmentCount,
                                    GetColorU32(ImGuizmoCol_Selection, 0.541f));
    // ... and outline
    g.DrawList->AddPolyline(circlePos, kArcSegmentCount,
                            GetColorU32(ImGuizmoCol_Selection), true, 2.0f);

    DrawRotationInfo(circlePos[1], moveType, g.Gizmo.RotationAngle);
  }
}

//-----------------------------------------------------------------------------
// [SECTION] SCALE
//-----------------------------------------------------------------------------

static ImGuizmoMoveType GetScaleType() {
  const ImGuizmoContext &g{ GImGuizmo };

  if (g.Gizmo.IsMouseOverOrigin()) return ImGuizmoMoveType_ScaleXYZ;

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 modelPosition{ g.Gizmo.ModelMatrix[3] };

    const glm::vec3 dirAxis{ g.Gizmo.ModelMatrix *
                             glm::vec4{ kUnitDirections[axis], 0.0f } };
    const float length{ IntersectRayPlane(g.Ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 mousePosOnPlane{ g.Ray.Start + g.Ray.Direction * length };
    const glm::vec2 mousePosOnPlaneInSS{ WorldToScreen(
      mousePosOnPlane, g.Camera.ViewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.ScreenFactor * 0.1f,
      g.Camera.ViewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * g.ScreenFactor,
      g.Camera.ViewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      mousePosOnPlaneInSS, axisStartOnScreen, axisEndOnScreen) };
    constexpr float kTolerance{ 8.0f };
    if (glm::length(closestPointOnAxis - mousePosOnPlaneInSS) < kTolerance)
      return ImGuizmoMoveType_ScaleX + axis;
  }

  return ImGuizmoMoveType_None;
}

static ImGuizmoMoveType BeginScale() {
  ImGuizmoContext &g{ GImGuizmo };

  ImGuizmoMoveType moveType{ GetScaleType() };
  if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();

  if (CanActivate() && moveType != ImGuizmoMoveType_None) {
    g.Gizmo.InUse = true;
    g.Gizmo.CurrentMoveType = moveType;

    const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
    const glm::vec3 movePlaneNormal[]{
      g.Gizmo.ModelMatrix[1], g.Gizmo.ModelMatrix[2], g.Gizmo.ModelMatrix[0],
      g.Gizmo.ModelMatrix[2], g.Gizmo.ModelMatrix[1], g.Gizmo.ModelMatrix[0],
      -g.Camera.Forward
    };
    g.Gizmo.TranslationPlane = BuildPlane(
      modelPosition, movePlaneNormal[moveType - ImGuizmoMoveType_ScaleX]);
    const float length{ IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane) };
    g.Gizmo.TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
    g.Gizmo.DragTranslationOrigin = modelPosition;
    g.Gizmo.Scale = glm::vec3{ 1.0f };
    g.Gizmo.ModelRelativeOrigin =
      (g.Gizmo.TranslationPlaneOrigin - modelPosition) *
      (1.0f / g.ScreenFactor);

    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      g.Gizmo.ScaleValueOrigin[axis] =
        glm::length(g.Gizmo.ModelSourceMatrix[axis]);

    g.DragOrigin = ImGui::GetIO().MousePos;
  }

  return moveType;
}
static bool ContinueScale(ImGuizmoMoveType &moveType, glm::mat4 &matrix,
                          float *deltaMatrix, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::CaptureMouseFromApp();

  const float length{ IntersectRayPlane(g.Ray, g.Gizmo.TranslationPlane) };
  const glm::vec3 newPos{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 newOrigin{ newPos -
                             g.Gizmo.ModelRelativeOrigin * g.ScreenFactor };
  const glm::vec3 &modelPosition{ g.Gizmo.ModelMatrix[3] };
  glm::vec3 delta{ newOrigin - modelPosition };

  // 1 axis constraint
  if (g.Gizmo.CurrentMoveType >= ImGuizmoMoveType_ScaleX &&
      g.Gizmo.CurrentMoveType <= ImGuizmoMoveType_ScaleZ) {
    const int axisIndex{ g.Gizmo.CurrentMoveType - ImGuizmoMoveType_ScaleX };
    const glm::vec3 axisValue{ g.Gizmo.ModelMatrix[axisIndex] };
    const float lengthOnAxis{ glm::dot(axisValue, delta) };
    delta = axisValue * lengthOnAxis;
    const glm::vec3 baseVector{ g.Gizmo.TranslationPlaneOrigin -
                                modelPosition };
    const float ratio{ glm::dot(axisValue, baseVector + delta) /
                       glm::dot(axisValue, baseVector) };
    g.Gizmo.Scale[axisIndex] = glm::max(ratio, 0.001f);
  } else {
    const float scaleDelta{ (ImGui::GetIO().MousePos.x - g.DragOrigin.x) *
                            0.01f };
    ImGui::Text("ScaleDelta = %.2f", scaleDelta);
    g.Gizmo.Scale = glm::vec3{ glm::max(1.0f + scaleDelta, 0.001f) };
  }

  if (snap) {
    const float scaleSnap[]{ snap[0], snap[0], snap[0] };
    ComputeSnap(g.Gizmo.Scale, scaleSnap);
  }

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    g.Gizmo.Scale[axis] = glm::max(g.Gizmo.Scale[axis], 0.001f);

  bool modified{ false };
  if (g.Gizmo.LastScale != g.Gizmo.Scale) modified = true;
  g.Gizmo.LastScale = g.Gizmo.Scale;

  glm::mat4 deltaMatrixScale{ glm::scale(g.Gizmo.Scale *
                                         g.Gizmo.ScaleValueOrigin) };
  matrix = g.Gizmo.ModelMatrix * deltaMatrixScale;

  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::scale(g.Gizmo.Scale);

  moveType = g.Gizmo.CurrentMoveType;
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

  if (!g.Gizmo.InUse) moveType = BeginScale();
  if (g.Gizmo.InUse) return ContinueScale(moveType, matrix, deltaMatrix, snap);

  return false;
}

static void DrawScaleGizmo(ImGuizmoMoveType moveType) {
  const ImGuizmoContext &g{ GImGuizmo };

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Scale);

  const glm::vec3 scaleDisplay{ g.Gizmo.InUse ? g.Gizmo.Scale
                                              : glm::vec3{ 1.0f } };

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const glm::vec3 dirAxis{ kUnitDirections[axis] };
    if (IsAxisVisible(dirAxis)) {
      const glm::vec2 tail{ WorldToScreen(dirAxis * 0.1f * g.ScreenFactor,
                                          g.Gizmo.ModelViewProjMatrix) };
      const glm::vec2 head{ WorldToScreen(dirAxis * g.ScreenFactor,
                                          g.Gizmo.ModelViewProjMatrix) };

      if (moveType != ImGuizmoMoveType_None && g.Gizmo.InUse) {
        g.DrawList->AddLine(tail, head, kDarkGrayColor, kLineThickness);
        g.DrawList->AddCircleFilled(head, kCircleRadius, kDarkGrayColor);
        const glm::vec2 headScaled{ WorldToScreen(
          (dirAxis * scaleDisplay[axis]) * g.ScreenFactor,
          g.Gizmo.ModelViewProjMatrix) };
        g.DrawList->AddLine(tail, headScaled, colors[axis + 1], kLineThickness);
        g.DrawList->AddCircleFilled(headScaled, kCircleRadius,
                                    colors[axis + 1]);
      } else {
        g.DrawList->AddLine(tail, head, colors[axis + 1], kLineThickness);
        g.DrawList->AddCircleFilled(head, kCircleRadius, colors[axis + 1]);
      }


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
  g.DrawList->AddCircleFilled(g.Gizmo.Origin, kCircleRadius, colors[0], 32);

  if (moveType != ImGuizmoMoveType_None && g.Gizmo.InUse)
    DrawScaleInfo(g.Gizmo.Origin, moveType, scaleDisplay);
}

//-----------------------------------------------------------------------------
// [SECTION] CAGE
//-----------------------------------------------------------------------------

/**
 * @param [in] bounds
 * @param [out] matrix
 * @param [in] snapValues
 */
static void HandleAndDrawLocalBounds(const float *bounds, glm::mat4 &matrix,
                                     const float *snapValues,
                                     ImGuizmoOperation_ operation) {
  ImGuizmoContext &g{ GImGuizmo };
  const ImGuiIO &io{ ImGui::GetIO() };

  // Compute best projection axis
  glm::vec3 axesWorldDirections[3]{};
  glm::vec3 bestAxisWorldDirection{ 0.0f };
  int axes[3]{ 0 };
  unsigned int numAxes{ 1 };
  axes[0] = g.Bounds.BestAxis;
  int bestAxis = axes[0];
  if (!g.Bounds.InUse) {
    numAxes = 0;
    float bestDot{ 0.0f };
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; axis++) {
      const glm::vec3 dirPlaneNormalWorld{ glm::normalize(
        g.Gizmo.ModelSourceMatrix * glm::vec4{ kUnitDirections[axis], 0.0f }) };

      const float dt{ glm::abs(
        glm::dot(glm::normalize(g.Camera.Eye -
                                glm::vec3{ g.Gizmo.ModelSourceMatrix[3] }),
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

    unsigned int anchorAlpha{ g.Enabled ? 0xFF000000 : 0x80000000 };

    const glm::mat4 boundsMVP{ g.Camera.ViewProjectionMatrix *
                               g.Gizmo.ModelSourceMatrix };

    for (int i = 0; i < 4; i++) {
      const glm::vec2 bound1{ WorldToScreen(aabb[i], boundsMVP) };
      const glm::vec2 bound2{ WorldToScreen(aabb[(i + 1) % 4], boundsMVP) };
      if (!g.Viewport.Contains(bound1) || !g.Viewport.Contains(bound2)) {
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
        g.DrawList->AddLine(tail, head, 0xAAAAAA + anchorAlpha, 2.0f);
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
        moveType = GetTranslateType();
        break;
      case ImGuizmoOperation_Rotate:
        moveType = GetRotateType();
        break;
      case ImGuizmoOperation_Scale:
        moveType = GetScaleType();
        break;

      case ImGuizmoOperation_Bounds:
        break;
      }

      if (moveType != ImGuizmoMoveType_None) {
        overBigAnchor = false;
        overSmallAnchor = false;
      }

      const auto kSelectionColor = 0x8A1080FF;
      auto bigAnchorColor = (overBigAnchor && g.Enabled)
                              ? kSelectionColor
                              : (0xAAAAAA + anchorAlpha);
      auto smallAnchorColor = (overSmallAnchor && g.Enabled)
                                ? kSelectionColor
                                : (0xAAAAAA + anchorAlpha);

      g.DrawList->AddCircleFilled(bound1, bigAnchorRadius, 0xFF000000);
      g.DrawList->AddCircleFilled(bound1, bigAnchorRadius - 1.2f,
                                  bigAnchorColor);

      g.DrawList->AddCircleFilled(midBound, smallAnchorRadius, 0xFF000000);
      g.DrawList->AddCircleFilled(midBound, smallAnchorRadius - 1.2f,
                                  smallAnchorColor);

      int oppositeIndex{ (i + 2) % 4 };
      // Big anchor on corners
      if (!g.Bounds.InUse && g.Enabled && overBigAnchor && CanActivate()) {
        g.Bounds.Pivot =
          g.Gizmo.ModelSourceMatrix * glm::vec4{ aabb[(i + 2) % 4], 1.0f };
        g.Bounds.Anchor =
          g.Gizmo.ModelSourceMatrix * glm::vec4{ aabb[i], 1.0f };

        g.Bounds.Plane = BuildPlane(g.Bounds.Anchor, bestAxisWorldDirection);
        g.Bounds.BestAxis = bestAxis;
        g.Bounds.Axis[0] = secondAxis;
        g.Bounds.Axis[1] = thirdAxis;

        g.Bounds.LocalPivot = glm::vec3{ 0.0f };
        g.Bounds.LocalPivot[secondAxis] = aabb[oppositeIndex][secondAxis];
        g.Bounds.LocalPivot[thirdAxis] = aabb[oppositeIndex][thirdAxis];

        g.Bounds.InUse = true;
        g.Bounds.Matrix = g.Gizmo.ModelSourceMatrix;
      }
      // Small anchor in the middle of the segment
      if (!g.Bounds.InUse && g.Enabled && overSmallAnchor && CanActivate()) {
        const glm::vec3 midPointOpposite{
          (aabb[(i + 2) % 4] + aabb[(i + 3) % 4]) * 0.5f
        };
        g.Bounds.Pivot =
          g.Gizmo.ModelSourceMatrix * glm::vec4{ midPointOpposite, 1.0f };
        g.Bounds.Anchor =
          g.Gizmo.ModelSourceMatrix * glm::vec4{ midPoint, 1.0f };

        g.Bounds.Plane = BuildPlane(g.Bounds.Anchor, bestAxisWorldDirection);
        g.Bounds.BestAxis = bestAxis;
        const int indices[]{ secondAxis, thirdAxis };
        g.Bounds.Axis[0] = indices[i % 2];
        g.Bounds.Axis[1] = -1;

        g.Bounds.LocalPivot = glm::vec3{ 0.0f };
        g.Bounds.LocalPivot[g.Bounds.Axis[0]] =
          aabb[oppositeIndex][indices[i % 2]];

        g.Bounds.InUse = true;
        g.Bounds.Matrix = g.Gizmo.ModelSourceMatrix;
      }
    }

    if (g.Bounds.InUse) {
      glm::mat4 scale{ 1.0f };

      // Compute projected mouse position on plane
      const float len{ IntersectRayPlane(g.Ray, g.Bounds.Plane) };
      const glm::vec3 newPos{ g.Ray.Start + g.Ray.Direction * len };

      // Compute a reference and delta vectors base on mouse move
      const glm::vec3 deltaVector{ glm::abs(newPos - g.Bounds.Pivot) };
      const glm::vec3 referenceVector{ glm::abs(g.Bounds.Anchor -
                                                g.Bounds.Pivot) };

      // For 1 or 2 axes, compute a ratio that's used for scale and snap it
      // based on resulting length
      for (int i = 0; i < 2; i++) {
        const int axisIndex1{ g.Bounds.Axis[i] };
        if (axisIndex1 == -1) continue;

        float ratioAxis{ 1.0f };
        const glm::vec3 axisDir{ glm::abs(g.Bounds.Matrix[axisIndex1]) };

        const float dtAxis{ glm::dot(axisDir, referenceVector) };
        const float boundSize{ bounds[axisIndex1 + 3] - bounds[axisIndex1] };
        if (dtAxis > kEpsilon)
          ratioAxis = glm::dot(axisDir, deltaVector) / dtAxis;

        if (snapValues) {
          float length{ boundSize * ratioAxis };
          ComputeSnap(length, snapValues[axisIndex1]);
          if (boundSize > kEpsilon) ratioAxis = length / boundSize;
        }
        scale[axisIndex1] *= ratioAxis;
      }

      // Transform matrix
      const glm::mat4 preScale{ glm::translate(-g.Bounds.LocalPivot) };
      const glm::mat4 postScale{ glm::translate(g.Bounds.LocalPivot) };
      matrix = g.Bounds.Matrix * postScale * scale * preScale;

      char scaleInfo[512]{};
      ImFormatString(scaleInfo, sizeof(scaleInfo), "X: %.2f Y: %.2f Z: %.2f",
                     (bounds[3] - bounds[0]) * glm::length(g.Bounds.Matrix[0]) *
                       glm::length(scale[0]),
                     (bounds[4] - bounds[1]) * glm::length(g.Bounds.Matrix[1]) *
                       glm::length(scale[1]),
                     (bounds[5] - bounds[2]) * glm::length(g.Bounds.Matrix[2]) *
                       glm::length(scale[2]));

      DrawText(g.Gizmo.Origin, scaleInfo);
    }

    if (!io.MouseDown[0]) {
      g.Bounds.InUse = false;
    }
    if (g.Bounds.InUse) break;
  }
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------
// [SECTION] PUBLIC INTERFACE
//-----------------------------------------------------------------------------

void PrintContext() {
  ImGuizmoContext &g{ GImGuizmo };

  ImGui::Begin("ImGuizmo::Debug");

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  ImGui::Checkbox("InUse", &g.Gizmo.InUse);
  ImGui::PopItemFlag();

  ImGui::Text("CurrentMoveType: %s",
              to_str(ImGuizmoMoveType_(g.Gizmo.CurrentMoveType)));
  // ImGui::Text("Operation: %s", to_str(g.gizmo.operation));


  if (ImGui::TreeNode("Gizmo")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Text("origin (screen space): [%0.2f, %0.2f]", g.Gizmo.Origin.x,
                g.Gizmo.Origin.y);
    ImGui::Text("ringRadius: %.2f", g.Gizmo.RingRadius);


    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

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
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("start", &g.Ray.Start[0], "%.2f");
    ImGui::InputFloat3("end", &g.Ray.End[0], "%.2f");
    ImGui::InputFloat3("direction", &g.Ray.Direction[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

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

  if (ImGui::TreeNode("Translation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("dragTranslationOrigin",
                       &g.Gizmo.DragTranslationOrigin[0]);
    ImGui::InputFloat3("lastTranslationDelta",
                       &g.Gizmo.LastTranslationDelta[0]);
    ImGui::InputFloat3("relativeOrigin", &g.Gizmo.ModelRelativeOrigin[0]);
    ImGui::InputFloat3("translationPlaneOrigin",
                       &g.Gizmo.TranslationPlaneOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Rotation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("rotationVectorSource",
                       &g.Gizmo.RotationVectorSource[0]);
    ImGui::InputFloat("rotationAngle", &g.Gizmo.RotationAngle);
    ImGui::InputFloat("rotationAngleOrigin", &g.Gizmo.RotationAngleOrigin);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Scale")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("modelScaleOrigin", &g.Gizmo.ModelScaleOrigin[0]);
    ImGui::InputFloat3("scaleValueOrigin", &g.Gizmo.ScaleValueOrigin[0]);
    ImGui::InputFloat3("scale", &g.Gizmo.Scale[0]);
    ImGui::InputFloat3("lastScale", &g.Gizmo.LastScale[0]);

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
  // ImGui::InputFloat3("axisFactor", &gContext.gizmo.axisDirectionFactor[0]);
  ImGui::PopItemFlag();

  ImGui::End();
}

void Enable(bool enabled) {
  GImGuizmo.Enabled = enabled;
  if (!enabled) {
    GImGuizmo.Gizmo.InUse = false;
    GImGuizmo.Bounds.InUse = false;
  }
}

void SetDrawlist(ImDrawList *drawList) {
  GImGuizmo.DrawList = drawList ? drawList : ImGui::GetWindowDrawList();
}
void SetViewport(const ImRect &viewport) {
  GImGuizmo.Viewport = viewport;
  //GImGuizmo.AspectRatio = viewport.GetWidth() / viewport.GetHeight();
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

  if (g.Enabled) {
    g.Ray = RayCast(g.Camera.ViewProjectionMatrix, g.Viewport.GetTL(),
                             g.Viewport.GetBR());
  }
}

void Begin(ImGuizmoMode_ mode, float *model) {
  IM_ASSERT(model);

  ImGuizmoContext &g{ GImGuizmo };
  IM_ASSERT(g.LockedModelMatrix == nullptr);

  g.Mode = mode;
  g.LockedModelMatrix = model;

  glm::mat4 modelMatrix{ glm::make_mat4(model) };
  if (g.Mode == ImGuizmoMode_Local) {
    g.Gizmo.ModelMatrix = modelMatrix;
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      g.Gizmo.ModelMatrix[axis] = glm::normalize(g.Gizmo.ModelMatrix[axis]);
  } else {
    g.Gizmo.ModelMatrix = glm::translate(glm::vec3{ modelMatrix[3] });
  }
  g.Gizmo.ModelViewProjMatrix =
    g.Camera.ViewProjectionMatrix * g.Gizmo.ModelMatrix;

  g.Gizmo.ModelSourceMatrix = modelMatrix;
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    g.Gizmo.ModelScaleOrigin[axis] =
      glm::length(g.Gizmo.ModelSourceMatrix[axis]);

  // ---

  g.Gizmo.Origin =
    WorldToScreen(glm::vec3{ 0.0f }, g.Gizmo.ModelViewProjMatrix);

  g.Gizmo.InversedModelMatrix = glm::inverse(g.Gizmo.ModelMatrix);
  const glm::vec3 rightViewInverse{ g.Gizmo.InversedModelMatrix *
                                    glm::vec4{ g.Camera.Right, 0.0f } };
  const float rightLength{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                     rightViewInverse) };
  g.ScreenFactor = g.Style.GizmoScale / rightLength;
}

void Translate(const float *snap, float *deltaMatrix) {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  if (CanTranslate(g.Gizmo.CurrentMoveType)) {
    g.Manipulated =
      HandleTranslation(moveType, g.Gizmo.ModelMatrix, deltaMatrix, snap);
  }
  DrawTranslationGizmo(moveType);
}

void Rotate(const float* snap, float* deltaMatrix) {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  if (CanRotate(g.Gizmo.CurrentMoveType)) {
    g.Manipulated =
      HandleRotation(moveType, g.Gizmo.ModelMatrix, deltaMatrix, snap);
  }
  DrawRotationGizmo(moveType);
}

void Scale(const float* snap, float* deltaMatrix) {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  if (CanScale(g.Gizmo.CurrentMoveType)) {
    g.Manipulated =
      HandleScale(moveType, g.Gizmo.ModelMatrix, deltaMatrix, snap);
  }
  DrawScaleGizmo(moveType);
}

void Cage(const float *bounds, const float *snap) {
  ImGuizmoContext &g{ GImGuizmo };

  HandleAndDrawLocalBounds(bounds, g.Gizmo.ModelMatrix, snap,
                           ImGuizmoOperation_Translate);
}

void End() {
  ImGuizmoContext &g{ GImGuizmo };
  IM_ASSERT(g.LockedModelMatrix && "It seems that you didn't call Begin()");
  if (g.Manipulated)
    *reinterpret_cast<glm::mat4 *>(g.LockedModelMatrix) = g.Gizmo.ModelMatrix;
  g.LockedModelMatrix = nullptr;

  if (!ImGui::GetIO().MouseDown[0]) {
    g.Gizmo.InUse = false;
    g.Gizmo.CurrentMoveType = ImGuizmoMoveType_None;
  }
}

bool Manipulate(ImGuizmoMode_ mode, ImGuizmoOperationFlags flags, float *model,
                float *deltaMatrix, const float *snap) {
  IM_ASSERT(model);

  ImGuizmoContext &g{ GImGuizmo };
  g.Mode = mode;

  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::mat4{ 1.0f };

  glm::mat4 modelMatrix{ glm::make_mat4(model) };
  if (g.Mode == ImGuizmoMode_Local) {
    g.Gizmo.ModelMatrix = modelMatrix;
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      g.Gizmo.ModelMatrix[axis] = glm::normalize(g.Gizmo.ModelMatrix[axis]);
  } else {
    g.Gizmo.ModelMatrix = glm::translate(glm::vec3{ modelMatrix[3] });
  }
  g.Gizmo.ModelViewProjMatrix =
    g.Camera.ViewProjectionMatrix * g.Gizmo.ModelMatrix;

  g.Gizmo.ModelSourceMatrix = modelMatrix;
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    g.Gizmo.ModelScaleOrigin[axis] =
      glm::length(g.Gizmo.ModelSourceMatrix[axis]);

  // ---

  g.Gizmo.Origin =
    WorldToScreen(glm::vec3{ 0.0f }, g.Gizmo.ModelViewProjMatrix);

  g.Gizmo.InversedModelMatrix = glm::inverse(g.Gizmo.ModelMatrix);
  const glm::vec3 rightViewInverse{ g.Gizmo.InversedModelMatrix *
                                    glm::vec4{ g.Camera.Right, 0.0f } };
  const float rightLength{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                     rightViewInverse) };
  g.ScreenFactor = g.Style.GizmoScale / rightLength;

  // ---

  const glm::vec3 camSpacePosition{ g.Gizmo.ModelViewProjMatrix *
                                    glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  if (!g.Camera.IsOrtho && camSpacePosition.z < 0.001f) return false;

  // ---

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  bool manipulated{ false };
  if (g.Enabled && !g.Bounds.InUse) {
    if (flags & ImGuizmoOperationFlags_Translate && CanTranslate(moveType)) {
      manipulated |=
        HandleTranslation(moveType, modelMatrix, deltaMatrix, snap);
    }
    if (flags & ImGuizmoOperationFlags_Rotate && CanRotate(moveType)) {
      manipulated |= HandleRotation(moveType, modelMatrix, deltaMatrix, snap);
    }
    if (flags & ImGuizmoOperationFlags_Scale && CanScale(moveType)) {
      manipulated |= HandleScale(moveType, modelMatrix, deltaMatrix, snap);
    }

    //if (!ImGui::GetIO().MouseDown[0]) g.gizmo.inUse = false;
  }

  /*
    if (localBounds && !g.gizmo.inUse) {
      priv::HandleAndDrawLocalBounds(localBounds, modelMatrix, boundsSnap,
                                     operation);
    }
    */

  *reinterpret_cast<glm::mat4 *>(model) = std::move(modelMatrix);

  //
  //
  //

  if (!g.Bounds.InUse) {
    if (flags & ImGuizmoOperationFlags_Translate) {
      DrawTranslationGizmo(IsTranslation(moveType) ? moveType
                                                   : ImGuizmoMoveType_None);
    }
    if (flags & ImGuizmoOperationFlags_Rotate) {
      DrawRotationGizmo(IsRotation(moveType) ? moveType
                                             : ImGuizmoMoveType_None);
    }
    if (flags & ImGuizmoOperationFlags_Scale) {
      DrawScaleGizmo(IsScale(moveType) ? moveType : ImGuizmoMoveType_None);
    }
  }

    if (!ImGui::GetIO().MouseDown[0]) g.Gizmo.InUse = false;


  return manipulated;
}

void ViewManipulate(float *view, const float length, ImVec2 position,
                    ImVec2 size, ImU32 backgroundColor) {
  const ImGuiIO &io{ ImGui::GetIO() };

  // ImDrawList *drawList{ gContext.drawList };
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

  const ImGuizmoRay ray{ RayCast(cubeProjection * cubeView, position,
                                          size) };

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
      const glm::vec3 indexVectorX{ kUnitDirections[perpXIndex] * invert };
      const glm::vec3 indexVectorY{ kUnitDirections[perpYIndex] * invert };
      const glm::vec3 boxOrigin{ kUnitDirections[normalIndex] * -invert -
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

bool IsUsing() { return GImGuizmo.Gizmo.InUse || GImGuizmo.Bounds.InUse; }
// bool IsOver() { return IsOver(GImGuizmo.gizmo.operation); }
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
          cubeFace_t *a = (cubeFace_t *)_a;
          cubeFace_t *b = (cubeFace_t *)_b;
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
    mat *= glm::rotate(glm::radians(rotation[i]), kUnitDirections[i]);

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const auto validScale =
      glm::abs(scale[axis]) < kEpsilon ? 0.001f : scale[axis];
    mat[axis] *= validScale;
  }
  mat[3] = glm::vec4{ glm::make_vec3(translation), 1.0f };
  *reinterpret_cast<glm::mat4 *>(matrix) = std::move(mat);
}

}; // namespace ImGuizmo

//-----------------------------------------------------------------------------
// [SECTION] ImGuizmoContext METHODS
//-----------------------------------------------------------------------------

float ImGuizmoContext::GetAspectRatio() const {
  return Viewport.GetWidth() / Viewport.GetHeight();
}

//-----------------------------------------------------------------------------
// [SECTION] ImGuizmoWidget METHODS
//-----------------------------------------------------------------------------

bool ImGuizmoWidget::IsMouseOverOrigin() const {
  const ImGuizmoContext &g{ GImGuizmo };

  constexpr float kMargin{ 10.0f };
  ImRect aabb{ Origin - kMargin, Origin + kMargin };
  return aabb.Contains(ImGui::GetIO().MousePos);
}
bool ImGuizmoWidget::IsMouseOverRotationRing() const {
  const ImGuizmoContext &g{ GImGuizmo };

  constexpr float kTolerance{ 1.0f };
  const float kRingThickness{ g.Style.RotationRingThickness + kTolerance };
  const float distance{ glm::length(glm::vec2{ ImGui::GetIO().MousePos } -
                                    Origin) };
  return (distance >= RingRadius - kRingThickness) &&
         (distance < RingRadius + kRingThickness);
}
