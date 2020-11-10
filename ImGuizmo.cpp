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

//
//
//

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

using ImGuizmoMode = int;
using ImGuizmoAxis = int;
using ImGuizmoMoveType = int;

enum ImGuizmoAxis_ {
  ImGuizmoAxis_X,
  ImGuizmoAxis_Y,
  ImGuizmoAxis_Z,

  ImGuizmoAxis_COUNT
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

struct ImGuizmoContext {
  void Compute(const glm::mat4 &view, const glm::mat4 &projection,
               const glm::mat4 &model, ImGuizmoMode_ mode);
  void SetupCamera(const glm::mat4 &view, const glm::mat4 &projection);
  void SetupModel(const glm::mat4 &model);
  void SetupGizmo();

  bool IsInsideViewport(const glm::vec2 &point) const;

  bool OverGizmoOrigin(const glm::vec2 &mousePosition) const;
  bool OverCameraAlignedRing(const glm::vec2 &mousePosition) const;

  glm::vec2 WorldToScreen(const glm::vec3 &worldPos,
                          const glm::mat4 &matrix) const;

  // ---

  void DrawText(const glm::vec2 &position, const char *text);

  //
  //
  //

  bool enabled{ true };

  ImDrawList *drawList{ nullptr };
  ImGuizmoStyle style{};

  struct {
    union {
      glm::vec2 position{ 0.0f }; // upper-left corner
      struct {
        float x, y;
      };
    };
    union {
      glm::vec2 size{ 0.0f };
      struct {
        float width, height;
      };
    };
  } viewport;
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
  float aspectRatio{ 1.0f };
  ImGuizmoRay ray{};

  glm::mat4 modelMatrix;
  glm::mat4 inversedModelMatrix;
  glm::mat4 modelViewProjMatrix;

  glm::mat4 modelSourceMatrix;

  struct {
    bool IsPresentlyUsed() const {
      return inUse && (actualID == -1 || actualID == editingID);
    }

    bool inUse{ false };

    int actualID{ -1 };
    int editingID{ -1 };

    ImGuizmoMode_ mode{ ImGuizmoMode_Local };
    ImGuizmoOperation_ operation{ ImGuizmoOperation_Translate };
    ImGuizmoMoveType currentMoveType{ ImGuizmoMoveType_None };

    glm::vec2 origin{ 0.0f };

    struct {
      glm::vec2 min, max;
    } aabb;
    float ringRadius{ 0.0f };

    // For translation and scale gizmo, either 1 or -1 (positive and negative
    // direction respectively)
    glm::vec3 axisDirectionFactor{ 1.0f, 1.0f, -1.0f };
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

  glm::vec3 modelScaleOrigin{ 1.0f };

  float screenFactor;
  glm::vec3 relativeOrigin{ 0.0f };

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
  bool belowAxisLimit[3];
  bool belowPlaneLimit[3];
};

static constexpr auto PI = glm::pi<float>();

static ImGuizmoContext gContext{};

const glm::vec3 kReferenceUp{ 0.0f, 1.0f, 0.0f };
static const glm::vec3 kUnitDirection[3]{ { 1.0f, 0.0f, 0.0f },
                                          { 0.0f, 1.0f, 0.0f },
                                          { 0.0f, 0.0f, 1.0f } };

constexpr auto kGrayColor = 0x999999;
constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

constexpr auto kLightGrayColor = 0xffdfdfdf;

constexpr float kRotationRingSize{ 4.5f };

namespace priv {

static const int kArcSegmentCount{ 64 };

static const char *kTranslationInfoMask[]{ "X : %5.3f",
                                           "Y : %5.3f",
                                           "Z : %5.3f",
                                           "Y : %5.3f Z : %5.3f",
                                           "X : %5.3f Z : %5.3f",
                                           "X : %5.3f Y : %5.3f",
                                           "X : %5.3f Y : %5.3f Z : %5.3f" };
static const char *kScaleInfoMask[]{ "X : %5.2f", "Y : %5.2f", "Z : %5.2f",
                                     "XYZ : %5.2f" };
static const char *kRotationInfoMask[]{ "X : %5.2f deg %5.2f rad",
                                        "Y : %5.2f deg %5.2f rad",
                                        "Z : %5.2f deg %5.2f rad",
                                        "Screen : %5.2f deg %5.2f rad" };
static const int kTranslationInfoIndex[]{ 0, 0, 0, 1, 0, 0, 2, 0, 0, 1, 2,
                                          0, 0, 2, 0, 0, 1, 0, 0, 1, 2 };

static const float kQuadMin{ 0.5f };
static const float kQuadMax{ 0.8f };
static const float kQuadUV[]{ kQuadMin, kQuadMin, kQuadMin, kQuadMax,
                              kQuadMax, kQuadMax, kQuadMax, kQuadMin };
//
//
//

const char *to_str(ImGuizmoOperation_ operation) {
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
const char *to_str(ImGuizmoMoveType_ mode) {
  switch (mode) {
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

// ---

static glm::vec4 BuildPlane(const glm::vec3 &point, const glm::vec3 &normal) {
  const auto n = glm::normalize(normal);
  return glm::vec4{ n, glm::dot(n, point) };
}

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
static float IntersectPlane(const ImGuizmoRay &ray, ImGuizmoPlane &plane) {}
static float IntersectRayPlane(const ImGuizmoRay &ray, const glm::vec4 &plane) {
  const float num{ glm::dot(glm::vec3{ plane }, ray.start) - plane.w };
  const float denom{ glm::dot(glm::vec3{ plane }, ray.direction) };

  // Normal is orthogonal to vector, cantt intersect
  if (glm::abs(denom) < FLT_EPSILON) return -1.0f;
  return -(num / denom);
}
static float DistanceToPlane(const glm::vec3 &point, const glm::vec4 &plane) {
  return glm::dot(glm::vec3{ plane }, point) + plane.w;
}

// ---

static ImU32 GetColorU32(ImGuiCol idx, float alpha_mul = 1.0f) {
  const ImGuizmoStyle &style{ gContext.style };
  ImVec4 c{ style.Colors[idx] };
  c.w *= style.Alpha * alpha_mul;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static ImU32 GetColorU32(const ImVec4 &col) {
  const ImGuizmoStyle &style{ gContext.style };
  ImVec4 c{ col };
  c.w *= style.Alpha;
  return ImGui::ColorConvertFloat4ToU32(c);
}
static const ImVec4 &GetStyleColorVec4(ImGuiCol idx) {
  const ImGuizmoStyle &style{ gContext.style };
  return style.Colors[idx];
}

// ---

static void DrawText(const glm::vec2 &position, const char *text) {
  assert(gContext.drawList != nullptr);
  gContext.drawList->AddText(position + 15.0f,
                             GetColorU32(ImGuizmoCol_TextShadow), text);
  gContext.drawList->AddText(position + 14.0f, GetColorU32(ImGuizmoCol_Text),
                             text);
}

// ---

static bool CanActivate() {
  if (ImGui::IsMouseClicked(0) && !ImGui::IsAnyItemHovered() &&
      !ImGui::IsAnyItemActive()) {
    return true;
  }
  return false;
}
/** @todo move mvp to paramlist */
static float GetSegmentLengthClipSpace(const glm::vec3 &start,
                                       const glm::vec3 &end) {
  auto startOfSegment = gContext.modelViewProjMatrix * glm::vec4{ start, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(startOfSegment.w) > FLT_EPSILON)
    startOfSegment *= 1.0f / startOfSegment.w;

  auto endOfSegment = gContext.modelViewProjMatrix * glm::vec4{ end, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(endOfSegment.w) > FLT_EPSILON)
    endOfSegment *= 1.0f / endOfSegment.w;

  glm::vec2 clipSpaceAxis{ endOfSegment - startOfSegment };
  clipSpaceAxis.y /= gContext.aspectRatio;
  return glm::length(clipSpaceAxis);
}

static float GetParallelogram(const glm::vec4 &ptO, const glm::vec4 &ptA,
                              const glm::vec4 &ptB) {
  glm::vec4 points[]{ ptO, ptA, ptB };
  for (auto i = 0; i < 3; ++i) {
    points[i] = gContext.modelViewProjMatrix * points[i];
    // Check for axis aligned with camera direction
    if (glm::abs(points[i].w) > FLT_EPSILON) points[i] *= 1.0f / points[i].w;
  }
  auto segA = points[1] - points[0];
  auto segB = points[2] - points[0];
  segA.y /= gContext.aspectRatio;
  segB.y /= gContext.aspectRatio;

  const auto segAOrtho =
    glm::normalize(glm::vec4{ -segA.y, segA.x, 0.0f, 0.0f });
  const float dt{ glm::dot(segAOrtho, segB) };
  const float surface{ glm::sqrt(segA.x * segA.x + segA.y * segA.y) *
                       glm::abs(dt) };
  return surface;
}

// ---

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

// ---

/**
 * @param [in] axisIndex
 * @param [out] dirAxis
 * @param [out] dirPlaneX
 * @param [out] dirPlaneY
 * @param [out] belowAxisLimit
 * @param [out] belowPlaneLimit
 */
static void
ComputeTripodAxisAndVisibility(ImGuizmoAxis axisIndex, glm::vec3 &dirAxis,
                               glm::vec3 &dirPlaneX, glm::vec3 &dirPlaneY,
                               bool &belowAxisLimit, bool &belowPlaneLimit) {
  assert(axisIndex >= ImGuizmoAxis_X && axisIndex < ImGuizmoAxis_COUNT);

  dirAxis = kUnitDirection[axisIndex];
  dirPlaneX = kUnitDirection[(axisIndex + 1) % 3];
  dirPlaneY = kUnitDirection[(axisIndex + 2) % 3];

  if (gContext.gizmo.IsPresentlyUsed()) {
    // When using, use stored factors so the gizmo doesn't flip when we
    // translate
    belowAxisLimit = gContext.belowAxisLimit[axisIndex];
    belowPlaneLimit = gContext.belowPlaneLimit[axisIndex];

    dirAxis *= gContext.gizmo.axisDirectionFactor[axisIndex];
    dirPlaneX *= gContext.gizmo.axisDirectionFactor[(axisIndex + 1) % 3];
    dirPlaneY *= gContext.gizmo.axisDirectionFactor[(axisIndex + 2) % 3];
  } else {
    static auto pickDirection = [](float dir, float negativeDir) {
      return (dir < negativeDir && glm::abs(dir - negativeDir) > FLT_EPSILON)
               ? -1.0f
               : 1.0f;
    };

    const glm::vec3 kOrigin{ 0.0f };

    const float lenDir{ GetSegmentLengthClipSpace(kOrigin, dirAxis) };
    const float lenNegativeDir{ GetSegmentLengthClipSpace(kOrigin, -dirAxis) };
    const float mulAxis{ pickDirection(lenDir, lenNegativeDir) };
    dirAxis *= mulAxis;

    const float lenDirPlaneX{ GetSegmentLengthClipSpace(kOrigin, dirPlaneX) };
    const float lenDirNegativePlaneX{ GetSegmentLengthClipSpace(kOrigin,
                                                                -dirPlaneX) };
    const float mulAxisX{ pickDirection(lenDirPlaneX, lenDirNegativePlaneX) };
    dirPlaneX *= mulAxisX;

    const float lenDirPlaneY{ GetSegmentLengthClipSpace(kOrigin, dirPlaneY) };
    const float lenDirNegativePlaneY{ GetSegmentLengthClipSpace(kOrigin,
                                                                -dirPlaneY) };
    const float mulAxisY{ pickDirection(lenDirPlaneY, lenDirNegativePlaneY) };
    dirPlaneY *= mulAxisY;

    const float axisLengthInClipSpace{ GetSegmentLengthClipSpace(
      kOrigin, dirAxis * gContext.screenFactor) };

    const float paraSurf{ GetParallelogram(
      glm::vec4{ 0.0f }, glm::vec4{ dirPlaneX, 0.0f } * gContext.screenFactor,
      glm::vec4{ dirPlaneY, 0.0f } * gContext.screenFactor) };
    belowPlaneLimit = paraSurf > 0.0025f;
    belowAxisLimit = axisLengthInClipSpace > 0.02f;

    gContext.gizmo.axisDirectionFactor[axisIndex] = mulAxis;
    gContext.gizmo.axisDirectionFactor[(axisIndex + 1) % 3] = mulAxisX;
    gContext.gizmo.axisDirectionFactor[(axisIndex + 2) % 3] = mulAxisY;
    gContext.belowAxisLimit[axisIndex] = belowAxisLimit;
    gContext.belowPlaneLimit[axisIndex] = belowPlaneLimit;
  }
}

static ImGuizmoMoveType GetTranslateType(const glm::vec2 &mousePosition) {
  if (gContext.OverGizmoOrigin(mousePosition))
    return ImGuizmoMoveType_MoveScreen;

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  for (int axis = ImGuizmoAxis_X;
       axis < ImGuizmoAxis_COUNT && moveType == ImGuizmoMoveType_None; ++axis) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(axis, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);

    dirAxis = gContext.modelMatrix * glm::vec4{ dirAxis, 0.0f };
    dirPlaneX = gContext.modelMatrix * glm::vec4{ dirPlaneX, 0.0f };
    dirPlaneY = gContext.modelMatrix * glm::vec4{ dirPlaneY, 0.0f };

    const glm::vec3 modelPosition{ gContext.modelMatrix[3] };
    const float length{ IntersectRayPlane(gContext.ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 posOnPlane{ gContext.ray.start +
                                gContext.ray.direction * length };

    const glm::vec2 posOnPlaneScreen{ gContext.WorldToScreen(
      posOnPlane, gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ gContext.WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor * 0.1f,
      gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ gContext.WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor,
      gContext.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      posOnPlaneScreen, axisStartOnScreen, axisEndOnScreen) };

    constexpr auto kTolerance = 8.0f;
    if (glm::length(closestPointOnAxis - posOnPlaneScreen) < kTolerance)
      moveType = ImGuizmoMoveType_MoveX + axis;

    const float dx{ glm::dot(dirPlaneX, (posOnPlane - modelPosition) *
                                          (1.0f / gContext.screenFactor)) };
    const float dy{ glm::dot(dirPlaneY, (posOnPlane - modelPosition) *
                                          (1.0f / gContext.screenFactor)) };

    if (belowPlaneLimit && dx >= kQuadUV[0] && dx <= kQuadUV[4] &&
        dy >= kQuadUV[1] && dy <= kQuadUV[3]) {
      moveType = ImGuizmoMoveType_MoveYZ + axis;
    }
  }

  return moveType;
}

static bool IsOverRotationAxis(const glm::vec2 &mousePosition,
                               ImGuizmoAxis axis) {
  const glm::vec4 pickupPlane{ BuildPlane(gContext.modelMatrix[3],
                                          gContext.modelMatrix[axis]) };
  const float length{ IntersectRayPlane(gContext.ray, pickupPlane) };
  const glm::vec3 localPos{ glm::normalize(
    gContext.ray.start + gContext.ray.direction * length -
    glm::vec3{ gContext.modelMatrix[3] }) };

  // ... ?
  if (glm::dot(localPos, gContext.ray.direction) > FLT_EPSILON) return false;

  const glm::vec3 idealPosOnCircle{ glm::mat3{ gContext.inversedModelMatrix } *
                                    localPos };
  const glm::vec2 idealPosOnCircleScreen{ gContext.WorldToScreen(
    idealPosOnCircle * gContext.screenFactor, gContext.modelViewProjMatrix) };

  constexpr auto kTolerance = 8.0f;
  const glm::vec2 distanceOnScreen{ idealPosOnCircleScreen - mousePosition };
  return glm::length(distanceOnScreen) < kTolerance;
}

static ImGuizmoMoveType GetRotateType(const glm::vec2 &mousePosition) {
  if (gContext.OverCameraAlignedRing(mousePosition))
    return ImGuizmoMoveType_RotateScreen;

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    if (IsOverRotationAxis(mousePosition, axis))
      return ImGuizmoMoveType_RotateX + axis;

  return ImGuizmoMoveType_None;
}
static ImGuizmoMoveType GetScaleType(const glm::vec2 &mousePosition) {
  if (gContext.OverGizmoOrigin(mousePosition)) return ImGuizmoMoveType_ScaleXYZ;

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  for (int axis = ImGuizmoAxis_X;
       axis < ImGuizmoAxis_COUNT && moveType == ImGuizmoMoveType_None; ++axis) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(axis, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);

    dirAxis = gContext.modelMatrix * glm::vec4{ dirAxis, 0.0f };
    dirPlaneX = gContext.modelMatrix * glm::vec4{ dirPlaneX, 0.0f };
    dirPlaneY = gContext.modelMatrix * glm::vec4{ dirPlaneY, 0.0f };

    const glm::vec3 modelPosition{ gContext.modelMatrix[3] };
    const float length{ IntersectRayPlane(gContext.ray,
                                          BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 posOnPlane{ gContext.ray.start +
                                gContext.ray.direction * length };

    const glm::vec2 posOnPlaneScreen{ gContext.WorldToScreen(
      posOnPlane, gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ gContext.WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor * 0.1f,
      gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ gContext.WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor,
      gContext.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      posOnPlaneScreen, axisStartOnScreen, axisEndOnScreen) };

    constexpr auto kTolerance = 12.0f;
    if (glm::length(closestPointOnAxis - posOnPlaneScreen) < kTolerance)
      moveType = ImGuizmoMoveType_ScaleX + axis;
  }

  return moveType;
}

static void ComputeColors(ImU32 colors[7], ImGuizmoMoveType moveType,
                          ImGuizmoOperation_ operation) {
  if (!gContext.enabled) {
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
  const float len{ IntersectRayPlane(gContext.ray, gContext.translationPlane) };
  const glm::vec3 localPos{ glm::normalize(
    gContext.ray.start + gContext.ray.direction * len -
    glm::vec3{ gContext.modelMatrix[3] }) };

  const glm::vec3 perpendicularVec{ glm::normalize(glm::cross(
    gContext.rotationVectorSource, glm::vec3{ gContext.translationPlane })) };

  const float acosAngle{ glm::clamp(
    glm::dot(localPos, gContext.rotationVectorSource), -1.0f, 1.0f) };
  float angle = glm::acos(acosAngle);
  angle *= (glm::dot(localPos, perpendicularVec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}

static void DrawTranslationGizmo(ImGuizmoMoveType moveType) {
  // Draw order:
  // 1. XYZ Axes (line + triangle)
  // 2. Planes (with border)
  // 3. Translation line with dots (from origin to destination)
  // 4. Circle at gizmo origin

  ImDrawList *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Translate);

  bool belowAxisLimit{ false };
  bool belowPlaneLimit{ false };
  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    ComputeTripodAxisAndVisibility(axis, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);
    // Draw line
    if (belowAxisLimit) {
      const glm::vec2 tail{ gContext.WorldToScreen(
        dirAxis * 0.1f * gContext.screenFactor, gContext.modelViewProjMatrix) };
      const glm::vec2 head{ gContext.WorldToScreen(
        dirAxis * gContext.screenFactor, gContext.modelViewProjMatrix) };

      constexpr float kLineThickness{ 3.0f };
      drawList->AddLine(tail, head, colors[axis + 1], kLineThickness);
      constexpr float kArrowheadSize{ kLineThickness * 2.0f };
      const glm::vec2 dir{ glm::normalize(gContext.gizmo.origin - head) *
                           kArrowheadSize };
      const glm::vec2 ortogonalDir{ dir.y, -dir.x }; // Perpendicular vector
      const glm::vec2 a{ head + dir };
      drawList->AddTriangleFilled(head - dir, a + ortogonalDir,
                                  a - ortogonalDir, colors[axis + 1]);
    }

    // Draw plane
    if (belowPlaneLimit) {
      ImVec2 screenQuadPts[4]{};
      for (int i = 0; i < 4; ++i) {
        const glm::vec3 cornerWorldSpace{ (dirPlaneX * kQuadUV[i * 2] +
                                           dirPlaneY * kQuadUV[i * 2 + 1]) *
                                          gContext.screenFactor };
        screenQuadPts[i] = gContext.WorldToScreen(cornerWorldSpace,
                                                  gContext.modelViewProjMatrix);
      }

      drawList->AddConvexPolyFilled(screenQuadPts, 4, colors[axis + 4]);
      constexpr float kQuadBorder{ 1.5f };
      drawList->AddPolyline(screenQuadPts, 4,
                            GetColorU32(ImGuizmoCol_AxisX + axis), true,
                            kQuadBorder);
    }
  }

  const float kCircleRadius{ 6.0f };
  if (gContext.gizmo.inUse &&
      (gContext.gizmo.actualID == -1 ||
       gContext.gizmo.actualID == gContext.gizmo.editingID)) {
    const glm::vec2 tail{ gContext.WorldToScreen(
      gContext.dragTranslationOrigin, gContext.camera.viewProjectionMatrix) };
    const glm::vec2 head{ gContext.WorldToScreen(
      gContext.modelMatrix[3], gContext.camera.viewProjectionMatrix) };
    const glm::vec2 diff{ glm::normalize(head - tail) *
                          (kCircleRadius - 1.0f) };

    constexpr auto kTranslationLineColor = 0xAAAAAAAA;
    constexpr float kMargin{ 1.5f };
    drawList->AddCircle(tail, kCircleRadius + kMargin, kTranslationLineColor);
    drawList->AddCircle(head, kCircleRadius + kMargin, kTranslationLineColor);
    drawList->AddLine(tail + diff, head - diff, kTranslationLineColor, 2.0f);

    const glm::vec3 deltaInfo{ glm::vec3{ gContext.modelMatrix[3] } -
                               gContext.dragTranslationOrigin };
    const int componentInfoIndex{ (moveType - ImGuizmoMoveType_MoveX) * 3 };
    char translationInfo[512]{};
    ImFormatString(translationInfo, sizeof(translationInfo),
                   kTranslationInfoMask[moveType - ImGuizmoMoveType_MoveX],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex]],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex + 1]],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex + 2]]);
    DrawText(head, translationInfo);
  }

  drawList->AddCircleFilled(gContext.gizmo.origin, kCircleRadius, colors[0],
                            32);
}
static void DrawRotationGizmo(ImGuizmoMoveType moveType) {
  ImDrawList *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Rotate);

  glm::vec3 cameraToModelNormalized;
  if (gContext.camera.isOrtho) {
    const glm::mat4 inversedViewMatrix{ glm::inverse(
      gContext.camera.viewMatrix) };
    cameraToModelNormalized = inversedViewMatrix[2];
  } else {
    cameraToModelNormalized = glm::normalize(
      glm::vec3{ gContext.modelMatrix[3] } - gContext.camera.eye);
  }
  // Always face to camera
  cameraToModelNormalized =
    gContext.inversedModelMatrix * glm::vec4{ cameraToModelNormalized, 0.0f };

  gContext.gizmo.ringRadius =
    gContext.style.ScreenRingSize * gContext.viewport.height;

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
                       gContext.screenFactor;
      circlePos[i] = gContext.WorldToScreen(pos, gContext.modelViewProjMatrix);
    }

    const float radiusAxis{ glm::length(gContext.gizmo.origin -
                                        glm::vec2{ circlePos[0] }) };
    if (radiusAxis > gContext.gizmo.ringRadius)
      gContext.gizmo.ringRadius = radiusAxis;

    drawList->AddCircleFilled(circlePos[0], 6.5f,
                              colors[ImGuizmoAxis_COUNT - axis], 32);

    drawList->AddPolyline(circlePos, kArcSegmentCount,
                          colors[ImGuizmoAxis_COUNT - axis], false, 2.0f);
  }

  // Circle parallel to view
  drawList->AddCircle(gContext.gizmo.origin, gContext.gizmo.ringRadius,
                      colors[0], kArcSegmentCount, kRotationRingSize);



  if (gContext.gizmo.inUse &&
      (gContext.gizmo.actualID == -1 ||
       gContext.gizmo.actualID == gContext.gizmo.editingID)) {
    ImVec2 circlePos[kArcSegmentCount + 1]{ gContext.gizmo.origin };
    for (int i = 1; i < kArcSegmentCount; ++i) {
      const float ng{ gContext.rotationAngle *
                      (static_cast<float>(i - 1) / (kArcSegmentCount - 1)) };

      const glm::mat3 rotateVectorMatrix{ glm::rotate(
        ng, glm::vec3{ gContext.translationPlane }) };
      glm::vec3 pos{ rotateVectorMatrix * gContext.rotationVectorSource };
      pos *= gContext.screenFactor;
      circlePos[i] =
        gContext.WorldToScreen(pos + glm::vec3{ gContext.modelMatrix[3] },
                               gContext.camera.viewProjectionMatrix);
    }

    // Draw inner part ...
    drawList->AddConvexPolyFilled(circlePos, kArcSegmentCount,
                                  GetColorU32(ImGuizmoCol_Selection, 0.541f));
    // ... and outline
    drawList->AddPolyline(circlePos, kArcSegmentCount,
                          GetColorU32(ImGuizmoCol_Selection), true, 2.0f);

    char rotationInfo[512]{};
    ImFormatString(rotationInfo, sizeof(rotationInfo),
                   kRotationInfoMask[moveType - ImGuizmoMoveType_RotateX],
                   glm::degrees(gContext.rotationAngle),
                   gContext.rotationAngle);
    DrawText(circlePos[1], rotationInfo);
  }
}
static void DrawScaleGizmo(ImGuizmoMoveType moveType) {
  constexpr auto kCircleRadius = 6.0f; // @todo move to global?

  ImDrawList *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, moveType, ImGuizmoOperation_Scale);

  glm::vec3 scaleDisplay{ 1.0f };
  if (gContext.gizmo.inUse &&
      (gContext.gizmo.actualID == -1 ||
       gContext.gizmo.actualID == gContext.gizmo.editingID)) {
    scaleDisplay = gContext.scale;
  }

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(axis, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);
    // Draw XYZ axes
    if (belowAxisLimit) {
      const glm::vec2 tail{ gContext.WorldToScreen(
        dirAxis * 0.1f * gContext.screenFactor, gContext.modelViewProjMatrix) };
      const glm::vec2 head{ gContext.WorldToScreen(
        dirAxis * gContext.screenFactor, gContext.modelViewProjMatrix) };
      const glm::vec2 headScaled{ gContext.WorldToScreen(
        (dirAxis * scaleDisplay[axis]) * gContext.screenFactor,
        gContext.modelViewProjMatrix) };

      constexpr auto kLineThickness = 3.0f;
      if (gContext.gizmo.inUse &&
          (gContext.gizmo.actualID == -1 ||
           gContext.gizmo.actualID == gContext.gizmo.editingID)) {
        drawList->AddLine(tail, head, kDarkGrayColor, kLineThickness);
        drawList->AddCircleFilled(head, kCircleRadius, kDarkGrayColor);
      }


      drawList->AddLine(tail, headScaled, colors[axis + 1], kLineThickness);
      drawList->AddCircleFilled(headScaled, kCircleRadius, colors[axis + 1]);
      
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
  drawList->AddCircleFilled(gContext.gizmo.origin, kCircleRadius, colors[0],
                            32);
  if (gContext.gizmo.inUse &&
      (gContext.gizmo.actualID == -1 ||
       gContext.gizmo.actualID == gContext.gizmo.editingID)) {
    char scaleInfo[512]{};
    const int componentInfoIndex{ (moveType - ImGuizmoMoveType_ScaleX) * 3 };
    ImFormatString(scaleInfo, sizeof(scaleInfo),
                   kScaleInfoMask[moveType - ImGuizmoMoveType_ScaleX],
                   scaleDisplay[kTranslationInfoIndex[componentInfoIndex]]);
    DrawText(gContext.gizmo.origin, scaleInfo);
  }
}

/**
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in/out] type
 * @param [in] snap
 */
static bool HandleTranslation(glm::mat4 &matrix, float *deltaMatrix,
                              ImGuizmoMoveType &moveType, const float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };

  bool modified{ false };

  if (gContext.gizmo.IsPresentlyUsed()) {
    ImGui::CaptureMouseFromApp();
    const float length{ glm::abs(
      IntersectRayPlane(gContext.ray, gContext.translationPlane)) };
    const glm::vec3 newPos{ gContext.ray.start +
                            gContext.ray.direction * length };

    ImGui::Text("length = %.2f", length);

    const glm::vec3 newOrigin{ newPos - gContext.relativeOrigin *
                                          gContext.screenFactor };
    glm::vec3 delta{ newOrigin - glm::vec3{ gContext.modelMatrix[3] } };

    // 1 axis constraint
    if (gContext.gizmo.currentMoveType >= ImGuizmoMoveType_MoveX &&
        gContext.gizmo.currentMoveType <= ImGuizmoMoveType_MoveZ) {
      const int axisIndex{ gContext.gizmo.currentMoveType -
                           ImGuizmoMoveType_MoveX };
      const glm::vec3 axisValue{ gContext.modelMatrix[axisIndex] };

      const float lengthOnAxis{ glm::dot(axisValue, delta) };
      delta = axisValue * lengthOnAxis;
    }

    if (snap) {
      glm::vec3 cumulativeDelta{ glm::vec3{ gContext.modelMatrix[3] } + delta -
                                 gContext.dragTranslationOrigin };
      bool applyRotationLocaly{ gContext.gizmo.mode == ImGuizmoMode_Local ||
                                moveType == ImGuizmoMoveType_MoveScreen };
      if (applyRotationLocaly) {
        glm::mat4 modelSourceNormalized{ gContext.modelSourceMatrix };
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
      delta = gContext.dragTranslationOrigin + cumulativeDelta -
              glm::vec3{ gContext.modelMatrix[3] };
    }

    if (delta != gContext.lastTranslationDelta) modified = true;
    gContext.lastTranslationDelta = delta;

    const glm::mat4 deltaMatrixTranslation{ glm::translate(delta) };
    if (deltaMatrix)
      *reinterpret_cast<glm::mat4 *>(deltaMatrix) = deltaMatrixTranslation;

    matrix = deltaMatrixTranslation * gContext.modelSourceMatrix;

    if (!io.MouseDown[0]) gContext.gizmo.inUse = false;
    moveType = gContext.gizmo.currentMoveType;
  } else {
    // Find new possible way to move
    moveType = GetTranslateType(io.MousePos);
    if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
    if (CanActivate() && moveType != ImGuizmoMoveType_None) {
      gContext.gizmo.inUse = true;
      gContext.gizmo.editingID = gContext.gizmo.actualID;
      gContext.gizmo.currentMoveType = moveType;
      glm::vec3 movePlaneNormal[]{
        gContext.modelMatrix[0], gContext.modelMatrix[1],
        gContext.modelMatrix[2], gContext.modelMatrix[0],
        gContext.modelMatrix[1], gContext.modelMatrix[2],
        -gContext.camera.forward
      };

      const glm::vec3 cameraToModelNormalized{ glm::normalize(
        glm::vec3{ gContext.modelMatrix[3] } - gContext.camera.eye) };
      for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
        const glm::vec3 orthoVector{ glm::cross(movePlaneNormal[axis],
                                                cameraToModelNormalized) };
        movePlaneNormal[axis] =
          glm::normalize(glm::cross(movePlaneNormal[axis], orthoVector));
      }

      gContext.translationPlane =
        BuildPlane(gContext.modelMatrix[3],
                   movePlaneNormal[moveType - ImGuizmoMoveType_MoveX]);

      const auto len =
        IntersectRayPlane(gContext.ray, gContext.translationPlane);
      gContext.translationPlaneOrigin =
        gContext.ray.start + gContext.ray.direction * len;
      gContext.dragTranslationOrigin = gContext.modelMatrix[3];
      gContext.relativeOrigin = (gContext.translationPlaneOrigin -
                                 glm::vec3{ gContext.modelMatrix[3] }) *
                                (1.0f / gContext.screenFactor);
    }
  }

  return modified;
}

/**
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in/out] type
 * @param [in] snap Angle in degrees
 */
static bool HandleRotation(glm::mat4 &matrix, float *deltaMatrix,
                           ImGuizmoMoveType &moveType, const float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };

  bool applyRotationLocaly{ gContext.gizmo.mode == ImGuizmoMode_Local };
  bool modified{ false };

  if (!gContext.gizmo.inUse) {
    moveType = GetRotateType(io.MousePos);
    if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
    if (moveType == ImGuizmoMoveType_RotateScreen) applyRotationLocaly = true;

    if (CanActivate() && moveType != ImGuizmoMoveType_None) {
      gContext.gizmo.inUse = true;
      gContext.gizmo.editingID = gContext.gizmo.actualID;
      gContext.gizmo.currentMoveType = moveType;

      const glm::vec3 rotatePlaneNormal[]{ gContext.modelMatrix[0],
                                           gContext.modelMatrix[1],
                                           gContext.modelMatrix[2],
                                           -gContext.camera.forward };
      if (applyRotationLocaly) {
        gContext.translationPlane =
          BuildPlane(gContext.modelMatrix[3],
                     rotatePlaneNormal[moveType - ImGuizmoMoveType_RotateX]);
      } else {
        gContext.translationPlane =
          BuildPlane(gContext.modelSourceMatrix[3],
                     kUnitDirection[moveType - ImGuizmoMoveType_RotateX]);
      }

      const float length{ IntersectRayPlane(gContext.ray,
                                            gContext.translationPlane) };
      const glm::vec3 localPos{ gContext.ray.start +
                                gContext.ray.direction * length -
                                glm::vec3{ gContext.modelMatrix[3] } };
      gContext.rotationVectorSource = glm::normalize(localPos);
      gContext.rotationAngleOrigin = ComputeAngleOnPlane();
    }
  }

  if (gContext.gizmo.IsPresentlyUsed()) {
    ImGui::CaptureMouseFromApp();
    gContext.rotationAngle = ComputeAngleOnPlane();
    if (snap) ComputeSnap(gContext.rotationAngle, glm::radians(snap[0]));

    const glm::vec3 rotationAxisLocalSpace{ glm::normalize(
      glm::mat3{ gContext.inversedModelMatrix } *
      glm::vec3{ gContext.translationPlane }) };

    const glm::mat4 deltaRotation{ glm::rotate(gContext.rotationAngle -
                                                 gContext.rotationAngleOrigin,
                                               rotationAxisLocalSpace) };

    if (gContext.rotationAngle != gContext.rotationAngleOrigin) modified = true;
    gContext.rotationAngleOrigin = gContext.rotationAngle;

    const glm::mat4 scaleOrigin{ glm::scale(gContext.modelScaleOrigin) };

    if (applyRotationLocaly) {
      matrix = gContext.modelMatrix * deltaRotation * scaleOrigin;
    } else {
      glm::mat4 result{ gContext.modelSourceMatrix };
      result[3] = glm::vec4{ glm::vec3{ 0.0f }, 1.0f };
      matrix = deltaRotation * result;
      matrix[3] = gContext.modelSourceMatrix[3];
    }

    if (deltaMatrix) {
      *reinterpret_cast<glm::mat4 *>(deltaMatrix) =
        gContext.modelMatrix * deltaRotation * gContext.inversedModelMatrix;
    }

    if (!io.MouseDown[0]) {
      gContext.gizmo.inUse = false;
      gContext.gizmo.editingID = -1;
    }
    moveType = gContext.gizmo.currentMoveType;
  }

  return modified;
}

/**
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [in/out] type
 * @param [in] snap
 */
static bool HandleScale(glm::mat4 &matrix, float *deltaMatrix,
                        ImGuizmoMoveType &moveType, const float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };
  static float dragOriginX{ 0.0f };

  const glm::vec3 modelPosition{ gContext.modelMatrix[3] };

  bool modified{ false };
  if (!gContext.gizmo.inUse) {
    moveType = GetScaleType(io.MousePos);
    if (moveType != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
    
    if (CanActivate() && moveType != ImGuizmoMoveType_None) {
      gContext.gizmo.inUse = true;
      gContext.gizmo.editingID = gContext.gizmo.actualID;
      gContext.gizmo.currentMoveType = moveType;

      const glm::vec3 movePlaneNormal[]{
        gContext.modelMatrix[1], gContext.modelMatrix[2],
        gContext.modelMatrix[0], gContext.modelMatrix[2],
        gContext.modelMatrix[1], gContext.modelMatrix[0],
        -gContext.camera.forward
      };
      gContext.translationPlane = BuildPlane(
        modelPosition, movePlaneNormal[moveType - ImGuizmoMoveType_ScaleX]);
      const float length{ IntersectRayPlane(gContext.ray,
                                         gContext.translationPlane) };
      gContext.translationPlaneOrigin =
        gContext.ray.start + gContext.ray.direction * length;
      gContext.dragTranslationOrigin = modelPosition;
      gContext.scale = glm::vec3{ 1.0f };
      gContext.relativeOrigin =
        (gContext.translationPlaneOrigin - modelPosition) *
        (1.0f / gContext.screenFactor);

      for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
        gContext.scaleValueOrigin[axis] =
          glm::length(gContext.modelSourceMatrix[axis]);
      }
      dragOriginX = io.MousePos.x;
    }
  }

  // ---

  if (gContext.gizmo.IsPresentlyUsed()) {
    ImGui::CaptureMouseFromApp();
    const float length{ IntersectRayPlane(gContext.ray,
                                          gContext.translationPlane) };
    const glm::vec3 newPos{ gContext.ray.start +
                            gContext.ray.direction * length };
    const glm::vec3 newOrigin{ newPos - gContext.relativeOrigin *
                                          gContext.screenFactor };
    glm::vec3 delta{ newOrigin - modelPosition };

    // 1 axis constraint
    if (gContext.gizmo.currentMoveType >= ImGuizmoMoveType_ScaleX &&
        gContext.gizmo.currentMoveType <= ImGuizmoMoveType_ScaleZ) {
      const int axisIndex{ gContext.gizmo.currentMoveType -
                           ImGuizmoMoveType_ScaleX };
      const glm::vec3 axisValue{ gContext.modelMatrix[axisIndex] };
      const float lengthOnAxis{ glm::dot(axisValue, delta) };
      delta = axisValue * lengthOnAxis;
      const glm::vec3 baseVector{ gContext.translationPlaneOrigin -
                                  modelPosition };
      const float ratio{ glm::dot(axisValue, baseVector + delta) /
                         glm::dot(axisValue, baseVector) };
      gContext.scale[axisIndex] = glm::max(ratio, 0.001f);
    } else {
      const float scaleDelta{ (io.MousePos.x - dragOriginX) * 0.01f };
      gContext.scale = glm::vec3{ glm::max(1.0f + scaleDelta, 0.001f) };
    }

    if (snap) {
      const float scaleSnap[]{ snap[0], snap[0], snap[0] };
      ComputeSnap(gContext.scale, scaleSnap);
    }

    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      gContext.scale[axis] = glm::max(gContext.scale[axis], 0.001f);

    if (gContext.lastScale != gContext.scale) modified = true;
    gContext.lastScale = gContext.scale;

    glm::mat4 deltaMatrixScale{ glm::scale(gContext.scale *
                                           gContext.scaleValueOrigin) };
    matrix = gContext.modelMatrix * deltaMatrixScale;

    if (deltaMatrix)
      *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::scale(gContext.scale);

    if (!io.MouseDown[0]) gContext.gizmo.inUse = false;
    moveType = gContext.gizmo.currentMoveType;
  }

  return modified;
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
  ImDrawList *drawList{ gContext.drawList };

  // Compute best projection axis
  glm::vec3 axesWorldDirections[3]{};
  glm::vec3 bestAxisWorldDirection{ 0.0f };
  int axes[3]{ 0 };
  unsigned int numAxes{ 1 };
  axes[0] = gContext.bounds.bestAxis;
  int bestAxis = axes[0];
  if (!gContext.bounds.inUse) {
    numAxes = 0;
    float bestDot{ 0.0f };
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; axis++) {
      const glm::vec3 dirPlaneNormalWorld{ glm::normalize(
        gContext.modelSourceMatrix * glm::vec4{ kUnitDirection[axis], 0.0f }) };

      const float dt{ glm::abs(
        glm::dot(glm::normalize(gContext.camera.eye -
                                glm::vec3{ gContext.modelSourceMatrix[3] }),
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

    unsigned int anchorAlpha{ gContext.enabled ? 0xFF000000 : 0x80000000 };

    const glm::mat4 boundsMVP{ gContext.camera.viewProjectionMatrix *
                               gContext.modelSourceMatrix };

    for (int i = 0; i < 4; i++) {
      const glm::vec2 bound1{ gContext.WorldToScreen(aabb[i], boundsMVP) };
      const glm::vec2 bound2{ gContext.WorldToScreen(aabb[(i + 1) % 4],
                                                     boundsMVP) };
      if (!gContext.IsInsideViewport(bound1) ||
          !gContext.IsInsideViewport(bound2)) {
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
      const glm::vec2 midBound{ gContext.WorldToScreen(midPoint, boundsMVP) };
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
      auto bigAnchorColor = (overBigAnchor && gContext.enabled)
                              ? kSelectionColor
                              : (0xAAAAAA + anchorAlpha);
      auto smallAnchorColor = (overSmallAnchor && gContext.enabled)
                                ? kSelectionColor
                                : (0xAAAAAA + anchorAlpha);

      drawList->AddCircleFilled(bound1, bigAnchorRadius, 0xFF000000);
      drawList->AddCircleFilled(bound1, bigAnchorRadius - 1.2f, bigAnchorColor);

      drawList->AddCircleFilled(midBound, smallAnchorRadius, 0xFF000000);
      drawList->AddCircleFilled(midBound, smallAnchorRadius - 1.2f,
                                smallAnchorColor);

      int oppositeIndex{ (i + 2) % 4 };
      // Big anchor on corners
      if (!gContext.bounds.inUse && gContext.enabled && overBigAnchor &&
          CanActivate()) {
        gContext.bounds.pivot =
          gContext.modelSourceMatrix * glm::vec4{ aabb[(i + 2) % 4], 1.0f };
        gContext.bounds.anchor =
          gContext.modelSourceMatrix * glm::vec4{ aabb[i], 1.0f };

        gContext.bounds.plane =
          BuildPlane(gContext.bounds.anchor, bestAxisWorldDirection);
        gContext.bounds.bestAxis = bestAxis;
        gContext.bounds.axis[0] = secondAxis;
        gContext.bounds.axis[1] = thirdAxis;

        gContext.bounds.localPivot = glm::vec3{ 0.0f };
        gContext.bounds.localPivot[secondAxis] =
          aabb[oppositeIndex][secondAxis];
        gContext.bounds.localPivot[thirdAxis] = aabb[oppositeIndex][thirdAxis];

        gContext.bounds.inUse = true;
        gContext.gizmo.editingID = gContext.gizmo.actualID;
        gContext.bounds.matrix = gContext.modelSourceMatrix;
      }
      // Small anchor in the middle of the segment
      if (!gContext.bounds.inUse && gContext.enabled && overSmallAnchor &&
          CanActivate()) {

        const glm::vec3 midPointOpposite{
          (aabb[(i + 2) % 4] + aabb[(i + 3) % 4]) * 0.5f
        };
        gContext.bounds.pivot =
          gContext.modelSourceMatrix * glm::vec4{ midPointOpposite, 1.0f };
        gContext.bounds.anchor =
          gContext.modelSourceMatrix * glm::vec4{ midPoint, 1.0f };

        gContext.bounds.plane =
          BuildPlane(gContext.bounds.anchor, bestAxisWorldDirection);
        gContext.bounds.bestAxis = bestAxis;
        const int indices[]{ secondAxis, thirdAxis };
        gContext.bounds.axis[0] = indices[i % 2];
        gContext.bounds.axis[1] = -1;

        gContext.bounds.localPivot = glm::vec3{ 0.0f };
        gContext.bounds.localPivot[gContext.bounds.axis[0]] =
          aabb[oppositeIndex][indices[i % 2]];

        gContext.bounds.inUse = true;
        gContext.gizmo.editingID = gContext.gizmo.actualID;
        gContext.bounds.matrix = gContext.modelSourceMatrix;
      }
    }

    if (gContext.bounds.inUse &&
        (gContext.gizmo.actualID == -1 ||
         gContext.gizmo.actualID == gContext.gizmo.editingID)) {
      glm::mat4 scale{ 1.0f };

      // Compute projected mouse position on plane
      const float len{ IntersectRayPlane(gContext.ray, gContext.bounds.plane) };
      const glm::vec3 newPos{ gContext.ray.start +
                              gContext.ray.direction * len };

      // Compute a reference and delta vectors base on mouse move
      const glm::vec3 deltaVector{ glm::abs(newPos - gContext.bounds.pivot) };
      const glm::vec3 referenceVector{ glm::abs(gContext.bounds.anchor -
                                                gContext.bounds.pivot) };

      // For 1 or 2 axes, compute a ratio that's used for scale and snap it
      // based on resulting length
      for (int i = 0; i < 2; i++) {
        const int axisIndex1{ gContext.bounds.axis[i] };
        if (axisIndex1 == -1) continue;

        float ratioAxis{ 1.0f };
        const glm::vec3 axisDir{ glm::abs(gContext.bounds.matrix[axisIndex1]) };

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
      const glm::mat4 preScale{ glm::translate(-gContext.bounds.localPivot) };
      const glm::mat4 postScale{ glm::translate(gContext.bounds.localPivot) };
      matrix = gContext.bounds.matrix * postScale * scale * preScale;

      char scaleInfo[512]{};
      ImFormatString(
        scaleInfo, sizeof(scaleInfo), "X: %.2f Y: %.2f Z: %.2f",
        (bounds[3] - bounds[0]) * glm::length(gContext.bounds.matrix[0]) *
          glm::length(scale[0]),
        (bounds[4] - bounds[1]) * glm::length(gContext.bounds.matrix[1]) *
          glm::length(scale[1]),
        (bounds[5] - bounds[2]) * glm::length(gContext.bounds.matrix[2]) *
          glm::length(scale[2]));

      DrawText(gContext.gizmo.origin, scaleInfo);
    }

    if (!io.MouseDown[0]) {
      gContext.bounds.inUse = false;
      gContext.gizmo.editingID = -1;
    }
    if (gContext.bounds.inUse) break;
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

//
// Public interface:
//

namespace ImGuizmo {

void PrintContext() {
  ImGui::Begin("ImGuizmo::Debug");

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  ImGui::Checkbox("InUse", &gContext.gizmo.inUse);
  ImGui::PopItemFlag();

  ImGui::Text("CurrentMoveType: %s",
              priv::to_str(ImGuizmoMoveType_(gContext.gizmo.currentMoveType)));
  ImGui::Text("Operation: %s", priv::to_str(gContext.gizmo.operation));

  ImGui::Text("ID, actual = %d, editing: %d", gContext.gizmo.actualID,
              gContext.gizmo.editingID);

  if (ImGui::TreeNode("Gizmo")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Text("origin (screen space): [%0.2f, %0.2f]",
                gContext.gizmo.origin.x, gContext.gizmo.origin.y);
    ImGui::Text("ringRadius: %.2f", gContext.gizmo.ringRadius);

    ImGui::Text("mScreenSquareMin: [%.2f, %.2f]", gContext.gizmo.aabb.min.x,
                gContext.gizmo.aabb.min.y);
    ImGui::Text("mScreenSquareMax: [%.2f, %.2f]", gContext.gizmo.aabb.max.x,
                gContext.gizmo.aabb.max.y);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Camera")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("Right", &gContext.camera.right[0], "%.2f");
    ImGui::InputFloat3("Up", &gContext.camera.up[0], "%.2f");
    ImGui::InputFloat3("Forward", &gContext.camera.forward[0], "%.2f");
    ImGui::InputFloat3("Eye", &gContext.camera.eye[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Ray")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("start", &gContext.ray.start[0], "%.2f");
    ImGui::InputFloat3("end", &gContext.ray.end[0], "%.2f");
    ImGui::InputFloat3("direction", &gContext.ray.direction[0], "%.2f");

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Bounds")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Checkbox("InUse", &gContext.bounds.inUse);
    ImGui::InputFloat3("Pivot", &gContext.bounds.pivot[0]);
    ImGui::InputFloat3("Anchor", &gContext.bounds.anchor[0]);
    ImGui::InputFloat3("LocalPivot", &gContext.bounds.localPivot[0]);
    ImGui::InputFloat4("Plane", &gContext.bounds.plane[0]);
    ImGui::InputInt("BestAxis", &gContext.bounds.bestAxis);
    ImGui::InputInt2("Axis", &gContext.bounds.axis[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Translation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("dragTranslationOrigin",
                       &gContext.dragTranslationOrigin[0]);
    ImGui::InputFloat3("lastTranslationDelta",
                       &gContext.lastTranslationDelta[0]);
    ImGui::InputFloat3("relativeOrigin", &gContext.relativeOrigin[0]);
    ImGui::InputFloat3("translationPlaneOrigin",
                       &gContext.translationPlaneOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Rotation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("rotationVectorSource",
                       &gContext.rotationVectorSource[0]);
    ImGui::InputFloat("rotationAngle", &gContext.rotationAngle);
    ImGui::InputFloat("rotationAngleOrigin", &gContext.rotationAngleOrigin);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Scale")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("modelScaleOrigin", &gContext.modelScaleOrigin[0]);
    ImGui::InputFloat3("scaleValueOrigin", &gContext.scaleValueOrigin[0]);
    ImGui::InputFloat3("scale", &gContext.scale[0]);
    ImGui::InputFloat3("lastScale", &gContext.lastScale[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
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
  ImGui::InputFloat3("axisFactor", &gContext.gizmo.axisDirectionFactor[0]);
  ImGui::PopItemFlag();

  ImGui::End();
}
ImGuizmoStyle &GetStyle() { return gContext.style; }

void Enable(bool enabled) {
  gContext.enabled = enabled;
  if (!enabled) {
    gContext.gizmo.inUse = false;
    gContext.bounds.inUse = false;
  }
}

void SetDrawlist(ImDrawList *drawList) {
  assert(drawList != nullptr);
  gContext.drawList = drawList ? drawList : ImGui::GetWindowDrawList();
}
void SetViewport(const ImVec2 &position, const ImVec2 &size) {
  gContext.viewport.position = position;
  gContext.viewport.size = size;
  gContext.aspectRatio = size.x / size.y;
}
void SetViewport(float x, float y, float width, float height) {
  SetViewport({ x, y }, { width, height });
}
void SetOrthographic(bool isOrthographic) {
  gContext.camera.isOrtho = isOrthographic;
}

void SpawnWorkspace(const char *name, const ImVec2 &position,
                           const ImVec2 &size) {
  ImGui::SetNextWindowPos(position);
  ImGui::SetNextWindowSize(size);
  ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
  ImGui::PushStyleColor(ImGuiCol_Border, 0);
  ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

  static const ImGuiWindowFlags flags{
    ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoResize |
    ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoInputs |
    ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing |
    ImGuiWindowFlags_NoBringToFrontOnFocus
  };
  ImGui::Begin(name, nullptr, flags);
  SetDrawlist(ImGui::GetWindowDrawList());
  ImGui::End();
  ImGui::PopStyleVar();
  ImGui::PopStyleColor(2);
}

void SetID(int id) { gContext.gizmo.actualID = id; }

/**
 * @param [in] view
 * @param [in] projection
 * @param [out] model
 * @param [out](optional) deltaMatrix
 * @param [in] snap
 * @param [in] localBounds
 * @param [in] boundsSnap
 */
bool Manipulate(const float *view, const float *projection,
                ImGuizmoOperation_ operation, ImGuizmoMode_ mode, float *model,
                float *deltaMatrix, const float *snap, const float *localBounds,
                const float *boundsSnap) {
  assert(view && projection && model);

  if (!ImGui::GetIO().WantCaptureMouse)
    gContext.gizmo.currentMoveType = ImGuizmoMoveType_None;

  glm::mat4 modelMatrix{ glm::make_mat4(model) };
  gContext.Compute(*reinterpret_cast<const glm::mat4 *>(view),
                   *reinterpret_cast<const glm::mat4 *>(projection),
                   modelMatrix, mode);

  if (deltaMatrix)
    *reinterpret_cast<glm::mat4 *>(deltaMatrix) = glm::mat4{ 1.0f };

  // ---

  const glm::vec3 camSpacePosition{ gContext.modelViewProjMatrix *
                                    glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  const bool isBehindCamera{ !gContext.camera.isOrtho &&
                             camSpacePosition.z < 0.001f };
  if (isBehindCamera) return false;

  // ---

  ImGuizmoMoveType moveType{ ImGuizmoMoveType_None };
  bool manipulated{ false };
  if (gContext.enabled && !gContext.bounds.inUse) {
    // Prevents operation switch while using
    if (gContext.gizmo.inUse && gContext.gizmo.operation != operation)
      operation = gContext.gizmo.operation;

    switch (operation) {
    case ImGuizmoOperation_Translate:
      manipulated =
        priv::HandleTranslation(modelMatrix, deltaMatrix, moveType, snap);
      break;
    case ImGuizmoOperation_Rotate:
      manipulated =
        priv::HandleRotation(modelMatrix, deltaMatrix, moveType, snap);
      break;
    case ImGuizmoOperation_Scale:
      manipulated = priv::HandleScale(modelMatrix, deltaMatrix, moveType, snap);
      break;

    case ImGuizmoOperation_Bounds:
      break;
    }
  }

  if (localBounds && !gContext.gizmo.inUse) {
    priv::HandleAndDrawLocalBounds(localBounds, modelMatrix, boundsSnap,
                                   operation);
  }

  *reinterpret_cast<glm::mat4 *>(model) = std::move(modelMatrix);

  //
  //
  //

  gContext.gizmo.operation = operation;
  if (!gContext.bounds.inUse) {
    switch (operation) {
    case ImGuizmoOperation_Rotate:
      priv::DrawRotationGizmo(moveType);
      break;
    case ImGuizmoOperation_Translate:
      priv::DrawTranslationGizmo(moveType);
      break;
    case ImGuizmoOperation_Scale:
      priv::DrawScaleGizmo(moveType);
      break;

    case ImGuizmoOperation_Bounds:
      break;
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

  const ImGuizmoRay ray{ priv::ScreenToWorldRay(
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
      const glm::vec4 viewSpaceFacePlane{ priv::BuildPlane(viewSpacePoint,
                                                           viewSpaceNormal) };

      if (viewSpaceFacePlane.w > 0) continue; // Back face culling

      const glm::vec4 facePlane{ priv::BuildPlane(n * 0.5f, n) };
      const float len{ priv::IntersectRayPlane(ray, facePlane) };
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
            priv::WorldToScreen((panelPos[iCoord] + origin) * 0.5f * invert,
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
        assert(boxCoordInt < 27);

        boxes[boxCoordInt] |= insidePanel && (!isDragging);

        // Draw face with lighter color
        if (iPass) {
          drawList->AddConvexPolyFilled(
            faceCoordsScreen, 4,
            (priv::GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0xff1f1f1f) |
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

bool IsUsing() { return gContext.gizmo.inUse || gContext.bounds.inUse; }
bool IsOver() { return IsOver(gContext.gizmo.operation); }
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
}


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
        const float dist{ priv::DistanceToPlane(centerPosition,
                                                frustum[iFrustum]) };
        if (dist < 0.0f) {
          inFrustum = false;
          break;
        }
      }
      if (!inFrustum) continue;

      cubeFace_t &cubeFace{ faces[cubeFaceCount] };
      for (int iCoord = 0; iCoord < 4; iCoord++) {
        cubeFace.faceCoordsScreen[iCoord] = gContext.WorldToScreen(
          faceCoords[iCoord] * 0.5f * invert, modelViewProjMatrix);
      }

      cubeFace.color =
        priv::GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0x808080;
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
    gContext.drawList->AddConvexPolyFilled(cubeFace.faceCoordsScreen, 4,
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
        float dA{ priv::DistanceToPlane(ptA, frustum[i]) };
        float dB{ priv::DistanceToPlane(ptB, frustum[i]) };
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

        gContext.drawList->AddLine(
          gContext.WorldToScreen(ptA, modelViewProjMatrix),
          gContext.WorldToScreen(ptB, modelViewProjMatrix), col, thickness);
      }
    }
  }
}

void DecomposeMatrix(const float *matrix, float *translation, float *rotation,
                     float *scale) {
  assert(matrix);

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
  assert(matrix && translation && rotation && scale);

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

//
//
//

void ImGuizmoContext::Compute(const glm::mat4 &view,
                              const glm::mat4 &projection,
                              const glm::mat4 &model, ImGuizmoMode_ mode) {
  gizmo.mode = mode;
  SetupCamera(view, projection);
  if (enabled) {
    ray = priv::ScreenToWorldRay(ImGui::GetIO().MousePos,
                                 camera.viewProjectionMatrix, viewport.position,
                                 viewport.size);
  }
  SetupModel(model);
  modelViewProjMatrix = camera.viewProjectionMatrix * modelMatrix;
  SetupGizmo();
}
void ImGuizmoContext::SetupCamera(const glm::mat4 &view,
                                  const glm::mat4 &projection) {
  camera.viewMatrix = view;
  camera.projectionMatrix = projection;
  camera.viewProjectionMatrix = projection * view;

  const glm::mat4 inversedViewMatrix{ glm::inverse(view) };
  camera.right = inversedViewMatrix[0];
  camera.up = inversedViewMatrix[1];
  camera.forward = inversedViewMatrix[2];
  camera.eye = inversedViewMatrix[3];
}
void ImGuizmoContext::SetupModel(const glm::mat4 &model) {
  modelSourceMatrix = model;
  if (gizmo.mode == ImGuizmoMode_Local) {
    modelMatrix = model;
    for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
      modelMatrix[axis] = glm::normalize(modelMatrix[axis]);
  } else {
    modelMatrix = glm::translate(glm::vec3{ model[3] });
  }
  inversedModelMatrix = glm::inverse(modelMatrix);

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis)
    modelScaleOrigin[axis] = glm::length(modelSourceMatrix[axis]);
}
void ImGuizmoContext::SetupGizmo() {
  gizmo.origin = WorldToScreen(glm::vec3{ 0.0f }, modelViewProjMatrix);
  constexpr float kMargin{ 10.0f };
  gizmo.aabb.min = gizmo.origin - kMargin;
  gizmo.aabb.max = gizmo.origin + kMargin;
  //gizmo.ringRadius = style.ScreenRingSize * viewport.height;

  const glm::vec3 rightViewInverse{ inversedModelMatrix *
                                    glm::vec4{ camera.right, 0.0f } };
  const float rightLength{ priv::GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                           rightViewInverse) };
  screenFactor = style.GizmoScale / rightLength;
}

bool ImGuizmoContext::IsInsideViewport(const glm::vec2 &point) const {
  ImRect vp{ viewport.position, viewport.position + viewport.size };
  return vp.Contains(point);
}
bool ImGuizmoContext::OverGizmoOrigin(const glm::vec2 &mousePosition) const {
  ImRect rect{ gizmo.aabb.min, gizmo.aabb.max };
  return rect.Contains(mousePosition);

  /*
  return glm::all(glm::greaterThanEqual(mousePosition, gizmo.aabb.min)) &&
         glm::all(glm::lessThanEqual(mousePosition, gizmo.aabb.max));
         */
}
bool ImGuizmoContext::OverCameraAlignedRing(
  const glm::vec2 &mousePosition) const {
  constexpr float kTolerance{ 1.0f };
  const glm::vec2 deltaScreen{ mousePosition - gizmo.origin };
  const float dist{ glm::length(deltaScreen) };
  return (dist >= (gizmo.ringRadius - (kRotationRingSize + kTolerance)) &&
          dist < (gizmo.ringRadius + (kRotationRingSize + kTolerance)));
}

glm::vec2 ImGuizmoContext::WorldToScreen(const glm::vec3 &worldPos,
                                         const glm::mat4 &matrix) const {
  return priv::WorldToScreen(worldPos, matrix, viewport.position,
                             viewport.size);
}

void ImGuizmoContext::DrawText(const glm::vec2 &position, const char *text) {
  assert(drawList != nullptr);
  drawList->AddText(position + 15.0f, priv::GetColorU32(ImGuizmoCol_TextShadow),
                    text);
  drawList->AddText(position + 14.0f, priv::GetColorU32(ImGuizmoCol_Text),
                    text);
}
