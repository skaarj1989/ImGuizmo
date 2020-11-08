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
#include "glm/gtx/compatibility.hpp"
#include "glm/gtc/type_ptr.hpp"

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

  const auto kRed = ImVec4{ 0.764f, 0.156f, 0.117f, 1.00f };
  const auto kGreen = ImVec4{ 0.411f, 0.686f, 0.098f, 1.00f };
  const auto kBlue = ImVec4{ 0.215f, 0.509f, 0.823f, 1.00f };

  colors[ImGuizmoCol_AxisX] = kRed;
  colors[ImGuizmoCol_AxisY] = kGreen;
  colors[ImGuizmoCol_AxisZ] = kBlue;

  colors[ImGuizmoCol_PlaneYZ] = kRed;
  colors[ImGuizmoCol_PlaneZX] = kGreen;
  colors[ImGuizmoCol_PlaneXY] = kBlue;
}

ImGuizmoStyle::ImGuizmoStyle() {
  StyleColorsDefault(this);
}

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

struct ImGuizmoContext {
  ImDrawList *drawList{ nullptr };

  ImGuizmoMode_ mode{ ImGuizmoMode_Local };

  struct {
    glm::mat4 viewMatrix;
    glm::mat4 projectionMatrix;
    glm::mat4 viewProjectionMatrix;

    glm::vec3 right;
    glm::vec3 up;
    glm::vec3 direction;
    glm::vec3 eye;
  } camera;

  glm::mat4 modelMatrix;
  glm::mat4 inversedModelMatrix;
  glm::mat4 modelViewProjMatrix;

  glm::mat4 modelSourceMatrix;
  glm::mat4 inversedModelSourceMatrix;

  struct {
    glm::vec4 origin;
    glm::vec4 direction;
  } ray;

  glm::vec3 modelScaleOrigin{ 1.0f };

  float mRadiusSquareCenter;

  glm::vec2 gizmoOrigin{ 0.0f };
  glm::vec2 mScreenSquareMin;
  glm::vec2 mScreenSquareMax;

  float screenFactor;
  glm::vec3 relativeOrigin{ 0.0f };

  bool inUse{ false };
  bool enabled{ true };

  // translation
  glm::vec4 translationPlane;
  glm::vec3 translationPlaneOrigin;
  glm::vec3 matrixOrigin;
  glm::vec3 lastTranslationDelta;

  // rotation
  glm::vec3 rotationVectorSource;
  float rotationAngle;
  float rotationAngleOrigin;
  // vec_t mWorldToLocalAxis;

  // scale
  glm::vec3 scale{ 1.0f };
  glm::vec3 scaleValueOrigin{ 1.0f };
  glm::vec3 lastScale{ 1.0f };

  float mSaveMousePosx;

  // save axis factor when using gizmo
  bool belowAxisLimit[3];
  bool belowPlaneLimit[3];
  float axisFactor[3];

  struct {
    glm::vec3 localPivot;
    glm::vec3 pivot;
    glm::vec3 anchor;
    glm::vec4 plane;
    int bestAxis;
    int axis[2];
    bool inUse{ false };
    glm::mat4 matrix;
  } bounds;

  int currentMoveType;

  struct {
    float x{ 0.0f };
    float y{ 0.0f };
    float width{ 0.0f };
    float height{ 0.0f };
  } viewport;

  float mXMax{ 0.0f };
  float mYMax{ 0.0f };
  float aspectRatio{ 1.0f };

  bool isOrtho{ false };

  int actualID{ -1 };
  int editingID{ -1 };
  ImGuizmoOperation_ operation{ ImGuizmoOperation_(-1) };

  ImGuizmoStyle style;
};

namespace ImGuizmo {

static constexpr auto PI = glm::pi<float>();
constexpr float kScreenRotateSize{ 0.06f };

static ImGuizmoContext gContext{};

constexpr glm::vec3 kReferenceUp{ 0.0f, 1.0f, 0.0f };

static const glm::vec3 kUnitDirection[3]{ { 1.0f, 0.0f, 0.0f },
                                           { 0.0f, 1.0f, 0.0f },
                                           { 0.0f, 0.0f, 1.0f } };

constexpr auto kBlackColor = 0xFF000000;
constexpr auto kGrayColor = 0x999999;
constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

constexpr auto kLightGrayColor = 0xffdfdfdf;

namespace priv {

static const int kArcSegmentCount{ 32 };
static const float kSnapTension{ 0.5f };

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

const char *to_str(ImGuizmoOperation_ operation) {
  switch (operation) {
  case ImGuizmoOperation_Translate:
    return "TRANSLATE";
  case ImGuizmoOperation_Rotate:
    return "ROTATE";
  case ImGuizmoOperation_Scale:
    return "SCALE";

  case ImGuizmoOperation_Bounds:
    return "BOUNDS";
  }

  return nullptr;
}
const char *to_str(ImGuizmoMoveType_ mode) {
  switch (mode) {
  case ImGuizmoMoveType_None:
    return "NONE";
  case ImGuizmoMoveType_MoveX:
    return "MOVE_X";
  case ImGuizmoMoveType_MoveY:
    return "MOVE_Y";
  case ImGuizmoMoveType_MoveZ:
    return "MOVE_Z";
  case ImGuizmoMoveType_MoveYZ:
    return "MOVE_YZ";
  case ImGuizmoMoveType_MoveZX:
    return "MOVE_ZX";
  case ImGuizmoMoveType_MoveXY:
    return "MOVE_XY";
  case ImGuizmoMoveType_MoveScreen:
    return "MOVE_SCREEN";

  case ImGuizmoMoveType_RotateX:
    return "ROTATE_X";
  case ImGuizmoMoveType_RotateY:
    return "ROTATE_Y";
  case ImGuizmoMoveType_RotateZ:
    return "ROTATE_Z";
  case ImGuizmoMoveType_RotateScreen:
    return "ROTATE_SCREEN";

  case ImGuizmoMoveType_ScaleX:
    return "SCALE_X";
  case ImGuizmoMoveType_ScaleY:
    return "SCALE_Y";
  case ImGuizmoMoveType_ScaleZ:
    return "SCALE_Z";
  case ImGuizmoMoveType_ScaleXYZ:
    return "SCALE_XYZ";
  }

  return nullptr;
}

// ---

static bool CanActivate() {
  if (ImGui::IsMouseClicked(0) && !ImGui::IsAnyItemHovered() &&
      !ImGui::IsAnyItemActive()) {
    return true;
  }
  return false;
}

template <typename T> bool IsWithin(T x, T y, T z) {
  return (x >= y) && (x <= z);
}
static bool IsInContextRect(const glm::vec2 &p) {
  return IsWithin(p.x, gContext.viewport.x, gContext.mXMax) &&
         IsWithin(p.y, gContext.viewport.y, gContext.mYMax);
}

static glm::vec4 BuildPlane(const glm::vec3 &point, const glm::vec3 &normal) {
  const auto n = glm::normalize(normal);
  return glm::vec4{ n, glm::dot(n, point) };
}
static glm::vec2 WorldToScreen(
  const glm::vec3 &worldPos, const glm::mat4 &mat,
  ImVec2 position = ImVec2{ gContext.viewport.x, gContext.viewport.y },
  ImVec2 size = ImVec2{ gContext.viewport.width, gContext.viewport.height }) {
  glm::vec4 screenPosition{ mat * glm::vec4{ worldPos, 1.0f } };
  screenPosition *= 0.5f / screenPosition.w;
  screenPosition += glm::vec4{ glm::vec2{ 0.5f }, 0.0f, 0.0f };
  screenPosition.y = 1.0f - screenPosition.y;
  screenPosition.x *= size.x;
  screenPosition.y *= size.y;
  screenPosition.x += position.x;
  screenPosition.y += position.y;
  return glm::vec2{ screenPosition };
}

// ---

ImU32 GetColorU32(ImGuiCol idx, float alpha_mul = 1.0f) {
  const ImGuizmoStyle &style{ gContext.style };
  ImVec4 c{ style.Colors[idx] };
  c.w *= style.Alpha * alpha_mul;
  return ImGui::ColorConvertFloat4ToU32(c);
}
ImU32 GetColorU32(const ImVec4 &col) {
  const ImGuizmoStyle &style{ gContext.style };
  ImVec4 c{ col };
  c.w *= style.Alpha;
  return ImGui::ColorConvertFloat4ToU32(c);
}
const ImVec4 &GetStyleColorVec4(ImGuiCol idx) {
  const ImGuizmoStyle &style{ gContext.style };
  return style.Colors[idx];
}

// ---

void DrawText(const glm::vec2 &position, const char *text) {
  ImDrawList *drawList{ gContext.drawList };
  drawList->AddText(position + 15.0f, GetColorU32(ImGuizmoCol_TextShadow),
                    text);
  drawList->AddText(position + 14.0f, GetColorU32(ImGuizmoCol_Text), text);
}

// ---

/** @todo remove gContext dependancy (move projection/view matrices to param list as inversedViewProj */
static void ComputeCameraRay(glm::vec4 &rayOrigin, glm::vec4 &rayDirection,
                             ImVec2 position = ImVec2{ gContext.viewport.x,
                                                       gContext.viewport.y },
                             ImVec2 size = ImVec2{ gContext.viewport.width,
                                                   gContext.viewport.height }) {
  const ImGuiIO &io{ ImGui::GetIO() };

  const glm::mat4 inversedViewProj{ glm::inverse(
    gContext.camera.projectionMatrix * gContext.camera.viewMatrix) };
  const float x{ ((io.MousePos.x - position.x) / size.x) * 2.0f - 1.0f };
  const float y{ (1.0f - ((io.MousePos.y - position.y) / size.y)) * 2.0f -
                 1.0f };

  rayOrigin = inversedViewProj * glm::vec4{ x, y, 0.0f, 1.0f };
  rayOrigin *= 1.0f / rayOrigin.w;

  auto rayEnd = inversedViewProj * glm::vec4{ x, y, 1.0f - FLT_EPSILON, 1.0f };
  rayEnd *= 1.0f / rayEnd.w;

  rayDirection = glm::normalize(rayEnd - rayOrigin);
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

  /*
  const float segmentLengthInClipSpace{ glm::sqrt(
    clipSpaceAxis.x * clipSpaceAxis.x + clipSpaceAxis.y * clipSpaceAxis.y) };
      const float slics = glm::length(glm::vec2{ clipSpaceAxis });
  assert(segmentLengthInClipSpace == slics);
  return segmentLengthInClipSpace;
  */
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

glm::vec2 PointOnSegment(const glm::vec2 &point, const glm::vec2 &v1,
                         const glm::vec2 &v2) {
  const auto c = point - v1;
  const auto V = glm::normalize(v2 - v1);
  const auto d = glm::length(v2 - v1);
  const auto t = glm::dot(V, c);

  if (t < 0) return v1;
  if (t > d) return v2;

  return v1 + V * t;
}

static float IntersectRayPlane(const glm::vec3 &rayOrigin,
                               const glm::vec3 &rayDirection,
                               const glm::vec4 &plane) {
  const float num{ glm::dot(glm::vec3{ plane }, rayOrigin) - plane.w };
  const float denom{ glm::dot(glm::vec3{ plane }, rayDirection) };

  // Normal is orthogonal to vector, cant intersect
  if (glm::abs(denom) < FLT_EPSILON) return -1.0f;
  return -(num / denom);
}
static float DistanceToPlane(const glm::vec3 &point, const glm::vec4 &plane) {
  return glm::dot(glm::vec3{ plane }, point) + plane.w;
}

// ---

static void ComputeSnap(float *value, float snap) {
  if (snap <= FLT_EPSILON) return;

#if 1
  float modulo{ fmodf(*value, snap) };
#else
  float modulo2{ glm::mod(*value, snap) };
  assert(modulo == modulo2); // fails on rotation ... why?
#endif

  const float moduloRatio{ glm::abs(modulo) / snap };
  if (moduloRatio < kSnapTension) {
    *value -= modulo;
  } else if (moduloRatio > (1.0f - kSnapTension)) {
    *value = *value - modulo + snap * ((*value < 0.0f) ? -1.0f : 1.0f);
  }
}
static void ComputeSnap(glm::vec3 &value, const float *snap) {
  for (int i = 0; i < 3; ++i)
    ComputeSnap(&value[i], snap[i]);
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
static void ComputeTripodAxisAndVisibility(int axisIndex, glm::vec3 &dirAxis,
                                           glm::vec3 &dirPlaneX,
                                           glm::vec3 &dirPlaneY,
                                           bool &belowAxisLimit,
                                           bool &belowPlaneLimit) {
  assert(axisIndex >= ImGuizmoAxis_X && axisIndex < ImGuizmoAxis_COUNT);

  dirAxis = kUnitDirection[axisIndex];
  dirPlaneX = kUnitDirection[(axisIndex + 1) % 3];
  dirPlaneY = kUnitDirection[(axisIndex + 2) % 3];

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    // When using, use stored factors so the gizmo doesn't flip when we
    // translate
    belowAxisLimit = gContext.belowAxisLimit[axisIndex];
    belowPlaneLimit = gContext.belowPlaneLimit[axisIndex];

    dirAxis *= gContext.axisFactor[axisIndex];
    dirPlaneX *= gContext.axisFactor[(axisIndex + 1) % 3];
    dirPlaneY *= gContext.axisFactor[(axisIndex + 2) % 3];
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

    /*
    const float mulAxis{ (lenDir < lenNegativeDir &&
                          glm::abs(lenDir - lenNegativeDir) > FLT_EPSILON)
                           ? -1.0f
                           : 1.0f };
    const float mulAxisX{ (lenDirPlaneX < lenDirMinusPlaneX &&
                          glm::abs(lenDirPlaneX - lenDirMinusPlaneX) >
                            FLT_EPSILON)
                           ? -1.0f
                           : 1.0f };
    const auto mulAxisY =
      (lenDirPlaneY < lenDirMinusPlaneY &&
       glm::abs(lenDirPlaneY - lenDirMinusPlaneY) > FLT_EPSILON)
        ? -1.0f
        : 1.0f;
        */

    const float axisLengthInClipSpace{ GetSegmentLengthClipSpace(
      kOrigin, dirAxis * gContext.screenFactor) };

    const float paraSurf{ GetParallelogram(
      glm::vec4{ 0.0f }, glm::vec4{ dirPlaneX, 0.0f } * gContext.screenFactor,
      glm::vec4{ dirPlaneY, 0.0f } * gContext.screenFactor) };
    belowPlaneLimit = paraSurf > 0.0025f;
    belowAxisLimit = axisLengthInClipSpace > 0.02f;

    gContext.axisFactor[axisIndex] = mulAxis;
    gContext.axisFactor[(axisIndex + 1) % 3] = mulAxisX;
    gContext.axisFactor[(axisIndex + 2) % 3] = mulAxisY;
    gContext.belowAxisLimit[axisIndex] = belowAxisLimit;
    gContext.belowPlaneLimit[axisIndex] = belowPlaneLimit;
  }
}

static int GetMoveType(glm::vec3 *gizmoHitProportion) {
  const ImGuiIO &io{ ImGui::GetIO() };
  int type{ ImGuizmoMoveType_None };

  if (io.MousePos.x >= gContext.mScreenSquareMin.x &&
      io.MousePos.x <= gContext.mScreenSquareMax.x &&
      io.MousePos.y >= gContext.mScreenSquareMin.y &&
      io.MousePos.y <= gContext.mScreenSquareMax.y) {
    type = ImGuizmoMoveType_MoveScreen;
  }

  for (int i = ImGuizmoAxis_X;
       i < ImGuizmoAxis_COUNT && type == ImGuizmoMoveType_None; ++i) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(i, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);

    dirAxis = gContext.modelMatrix * glm::vec4{ dirAxis, 0.0f };
    dirPlaneX = gContext.modelMatrix * glm::vec4{ dirPlaneX, 0.0f };
    dirPlaneY = gContext.modelMatrix * glm::vec4{ dirPlaneY, 0.0f };

    const glm::vec3 modelPosition{ gContext.modelMatrix[3] };
    const float len{ IntersectRayPlane(gContext.ray.origin,
                                       gContext.ray.direction,
                                       BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 posOnPlane{ gContext.ray.origin +
                                gContext.ray.direction * len };

    const glm::vec2 posOnPlaneScreen{ WorldToScreen(
      posOnPlane, gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor * 0.1f,
      gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor,
      gContext.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      posOnPlaneScreen, axisStartOnScreen, axisEndOnScreen) };

    constexpr auto kPixelSize = 12.0f;
    if (glm::length(closestPointOnAxis - posOnPlaneScreen) < kPixelSize)
      type = ImGuizmoMoveType_MoveX + i;

    const float dx{ glm::dot(dirPlaneX, (posOnPlane - modelPosition) *
                                          (1.0f / gContext.screenFactor)) };
    const float dy{ glm::dot(dirPlaneY, (posOnPlane - modelPosition) *
                                          (1.0f / gContext.screenFactor)) };

    if (belowPlaneLimit && dx >= kQuadUV[0] && dx <= kQuadUV[4] &&
        dy >= kQuadUV[1] && dy <= kQuadUV[3]) {
      type = ImGuizmoMoveType_MoveYZ + i;
    }
    if (gizmoHitProportion) *gizmoHitProportion = glm::vec3{ dx, dy, 0.0f };
  }

  return type;
}
static int GetRotateType() {
  const ImGuiIO &io{ ImGui::GetIO() };
  int type{ ImGuizmoMoveType_None };

  const glm::vec2 deltaScreen{ io.MousePos - gContext.gizmoOrigin };
  const float dist{ glm::length(deltaScreen) };
  if (dist >= (gContext.mRadiusSquareCenter - 1.0f) &&
      dist < (gContext.mRadiusSquareCenter + 1.0f)) {
    type = ImGuizmoMoveType_RotateScreen;
  }

  const glm::vec3 planeNormals[]{ gContext.modelMatrix[0],
                                  gContext.modelMatrix[1],
                                  gContext.modelMatrix[2] };

  for (int i = ImGuizmoAxis_X;
       i < ImGuizmoAxis_COUNT && type == ImGuizmoMoveType_None; ++i) {
    const glm::vec4 pickupPlane{ BuildPlane(gContext.modelMatrix[3],
                                            planeNormals[i]) };

    const float len{ IntersectRayPlane(gContext.ray.origin,
                                       gContext.ray.direction, pickupPlane) };
    const glm::vec3 localPos{ glm::normalize(gContext.ray.origin +
                                             gContext.ray.direction * len -
                                             gContext.modelMatrix[3]) };

    if (glm::dot(localPos, glm::vec3{ gContext.ray.direction }) > FLT_EPSILON)
      continue;
    
    const glm::vec3 idealPosOnCircle{ gContext.inversedModelMatrix *
                                      glm::vec4{ localPos, 0.0f } };
    const glm::vec2 idealPosOnCircleScreen{ WorldToScreen(
      idealPosOnCircle * gContext.screenFactor, gContext.modelViewProjMatrix) };

    constexpr auto kPixelSize = 8.0f;
#if 0
    gContext.drawList->AddCircle(idealPosOnCircleScreen, kPixelSize,
                                 GetColorU32(ImGuizmoCol_Selection, 0.541f));
#endif

    const glm::vec2 distanceOnScreen{ idealPosOnCircleScreen - io.MousePos };
    if (glm::length(distanceOnScreen) < kPixelSize)
      type = ImGuizmoMoveType_RotateX + i;
  }

  return type;
}
static int GetScaleType() {
  const ImGuiIO &io{ ImGui::GetIO() };
  int type{ ImGuizmoMoveType_None };

  if (io.MousePos.x >= gContext.mScreenSquareMin.x &&
      io.MousePos.x <= gContext.mScreenSquareMax.x &&
      io.MousePos.y >= gContext.mScreenSquareMin.y &&
      io.MousePos.y <= gContext.mScreenSquareMax.y) {
    type = ImGuizmoMoveType_ScaleXYZ;
  }

  for (int i = ImGuizmoAxis_X;
       i < ImGuizmoAxis_COUNT && type == ImGuizmoMoveType_None; ++i) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(i, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);

    dirAxis = gContext.modelMatrix * glm::vec4{ dirAxis, 0.0f };
    dirPlaneX = gContext.modelMatrix * glm::vec4{ dirPlaneX, 0.0f };
    dirPlaneY = gContext.modelMatrix * glm::vec4{ dirPlaneY, 0.0f };

    const glm::vec3 modelPosition{ gContext.modelMatrix[3] };

    const float len{ IntersectRayPlane(gContext.ray.origin,
                                       gContext.ray.direction,
                                       BuildPlane(modelPosition, dirAxis)) };
    const glm::vec3 posOnPlane{ gContext.ray.origin +
                                gContext.ray.direction * len };

    const glm::vec2 posOnPlaneScreen{ WorldToScreen(
      posOnPlane, gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisStartOnScreen{ WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor * 0.1f,
      gContext.camera.viewProjectionMatrix) };
    const glm::vec2 axisEndOnScreen{ WorldToScreen(
      modelPosition + dirAxis * gContext.screenFactor,
      gContext.camera.viewProjectionMatrix) };

    const glm::vec2 closestPointOnAxis{ PointOnSegment(
      posOnPlaneScreen, axisStartOnScreen, axisEndOnScreen) };

    constexpr auto kPixelSize = 12.0f;
    if (glm::length(closestPointOnAxis - posOnPlaneScreen) < kPixelSize)
      type = ImGuizmoMoveType_ScaleX + i;
  }

  return type;
}

static void ComputeContext(const glm::mat4 &view, const glm::mat4 &projection,
                           const glm::mat4 &model, ImGuizmoMode_ mode) {
  gContext.mode = mode;

  gContext.camera.viewMatrix = view;
  gContext.camera.projectionMatrix = projection;
  gContext.camera.viewProjectionMatrix =
    gContext.camera.projectionMatrix * gContext.camera.viewMatrix;

  const glm::mat4 inversedViewMatrix{ glm::inverse(
    gContext.camera.viewMatrix) };
  gContext.camera.right = inversedViewMatrix[0];
  gContext.camera.up = inversedViewMatrix[1];
  gContext.camera.direction = inversedViewMatrix[2];
  gContext.camera.eye = inversedViewMatrix[3];

  gContext.modelSourceMatrix = model;
  gContext.inversedModelSourceMatrix = glm::inverse(gContext.modelSourceMatrix);

  if (mode == ImGuizmoMode_Local) {
    gContext.modelMatrix = model;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i)
      gContext.modelMatrix[i] = glm::normalize(gContext.modelMatrix[i]);
  } else {
    gContext.modelMatrix =
      glm::translate(glm::mat4{ 1.0f }, glm::vec3{ model[3] });
  }
  gContext.inversedModelMatrix = glm::inverse(gContext.modelMatrix);

  gContext.modelViewProjMatrix =
    gContext.camera.viewProjectionMatrix * gContext.modelMatrix;

  gContext.modelScaleOrigin =
    glm::vec3{ glm::length(gContext.modelSourceMatrix[0]),
               glm::length(gContext.modelSourceMatrix[1]),
               glm::length(gContext.modelSourceMatrix[2]) };

  // ---

  const float kGizmoScale{ gContext.style.GizmoScale };

  // Compute scale from the size of camera right vector projected on screen at
  // the matrix position
  const glm::vec4 pointRight{ gContext.camera.viewProjectionMatrix *
                              glm::vec4{ gContext.camera.right, 0.0f } };
  gContext.screenFactor = kGizmoScale / (pointRight.x / pointRight.w -
                                         gContext.modelViewProjMatrix[3].x /
                                           gContext.modelViewProjMatrix[3].w);
  const glm::vec3 rightViewInverse{ gContext.inversedModelMatrix *
                                    glm::vec4{ gContext.camera.right, 0.0f } };
  const float rightLength{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                     rightViewInverse) };
  gContext.screenFactor = kGizmoScale / rightLength;

  const glm::vec2 centerScreenSpace{ WorldToScreen(
    glm::vec3{ 0.0f }, gContext.modelViewProjMatrix) };
  gContext.gizmoOrigin = centerScreenSpace;

  constexpr float kMargin{ 10.0f };
  gContext.mScreenSquareMin = centerScreenSpace - kMargin;
  gContext.mScreenSquareMax = centerScreenSpace + kMargin;

  ComputeCameraRay(gContext.ray.origin, gContext.ray.direction);
}

static void ComputeColors(ImU32 colors[7], int type,
                          ImGuizmoOperation_ operation) {
  if (!gContext.enabled) {
    for (auto i = 0; i < 7; ++i)
      colors[i] = GetColorU32(ImGuizmoCol_Inactive);
    return;
  }

  const auto kWhiteColor = 0xFFFFFFFF;
  const auto kSelectionColor = GetColorU32(ImGuizmoCol_Selection, 0.541f);
  switch (operation) {
  case ImGuizmoOperation_Translate:
    colors[0] =
      (type == ImGuizmoMoveType_MoveScreen) ? kSelectionColor : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (type == (ImGuizmoMoveType_MoveX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
      colors[i + 4] = (type == (ImGuizmoMoveType_MoveYZ + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_PlaneYZ + i);
      colors[i + 4] =
        (type == ImGuizmoMoveType_MoveScreen) ? kSelectionColor : colors[i + 4];
    }
    break;
  case ImGuizmoOperation_Rotate:
    colors[0] =
      (type == ImGuizmoMoveType_RotateScreen) ? kSelectionColor : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (type == (ImGuizmoMoveType_RotateX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
    }
    break;
  case ImGuizmoOperation_Scale:
    colors[0] =
      (type == ImGuizmoMoveType_ScaleXYZ) ? kSelectionColor : kWhiteColor;
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
      colors[i + 1] = (type == (ImGuizmoMoveType_ScaleX + i))
                        ? kSelectionColor
                        : GetColorU32(ImGuizmoCol_AxisX + i);
    }
    break;

  case ImGuizmoOperation_Bounds:
    break;
  }
}


static float ComputeAngleOnPlane() {
  const float len{ IntersectRayPlane(
    gContext.ray.origin, gContext.ray.direction, gContext.translationPlane) };
  const glm::vec3 localPos{ glm::normalize(gContext.ray.origin +
                                           gContext.ray.direction * len -
                                           gContext.modelMatrix[3]) };

  const glm::vec3 perpendicularVec{ glm::normalize(glm::cross(
    gContext.rotationVectorSource, glm::vec3{ gContext.translationPlane })) };

  const float acosAngle{ glm::clamp(
    glm::dot(localPos, gContext.rotationVectorSource), -1.0f, 1.0f) };
  float angle = glm::acos(acosAngle);
  angle *= (glm::dot(localPos, perpendicularVec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}

static void DrawHatchedAxis(const glm::vec3 &axis) {
   // dont like it ...
   return;

  for (int j = 1; j < 10; ++j) {
    ImVec2 baseSSpace2 = WorldToScreen(
      axis * 0.05f * static_cast<float>(j * 2) * gContext.screenFactor,
      gContext.modelViewProjMatrix);
    ImVec2 worldDirSSpace2 = WorldToScreen(
      axis * 0.05f * static_cast<float>(j * 2 + 1) * gContext.screenFactor,
      gContext.modelViewProjMatrix);
    gContext.drawList->AddLine(baseSSpace2, worldDirSSpace2, 0x80000000, 6.0f);
  }
}

static void DrawTranslationGizmo(int type) {
  // assert(type != MOVETYPE::NONE);

  ImDrawList *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, type, ImGuizmoOperation_Translate);

  const glm::vec2 gizmoOriginScreenSpace{ WorldToScreen(
    gContext.modelMatrix[3], gContext.camera.viewProjectionMatrix) };

  bool belowAxisLimit{ false };
  bool belowPlaneLimit{ false };
  for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    ComputeTripodAxisAndVisibility(i, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);

    // Draw axis
    if (belowAxisLimit) {
      const glm::vec2 tail{ WorldToScreen(
        dirAxis * 0.1f * gContext.screenFactor, gContext.modelViewProjMatrix) };
      const glm::vec2 head{ WorldToScreen(dirAxis * gContext.screenFactor,
                                          gContext.modelViewProjMatrix) };

      drawList->AddLine(tail, head, colors[i + 1], 3.0f);

      #if 1
      glm::vec2 dir{ glm::normalize(gizmoOriginScreenSpace - head) * 6.0f };
      #else
      glm::vec2 dir{ gizmoOriginScreenSpace - head };
      float d = sqrtf(ImLengthSqr(dir));
      dir /= d; // Normalize
      dir *= 6.0f;
      #endif

      const glm::vec2 ortogonalDir{ dir.y, -dir.x }; // Perpendicular vector
      const glm::vec2 a{ head + dir };
      drawList->AddTriangleFilled(head - dir, a + ortogonalDir,
                                  a - ortogonalDir, colors[i + 1]);
      // Arrow head end
      if (gContext.axisFactor[i] < 0) DrawHatchedAxis(dirAxis);
    }

    // Draw plane
    if (belowPlaneLimit) {
      ImVec2 screenQuadPts[4]{};
      for (int j = 0; j < 4; ++j) {
        const glm::vec3 cornerWorldSpace{ (dirPlaneX * kQuadUV[j * 2] +
                                           dirPlaneY * kQuadUV[j * 2 + 1]) *
                                          gContext.screenFactor };
        screenQuadPts[j] =
          WorldToScreen(cornerWorldSpace, gContext.modelViewProjMatrix);
      }

      drawList->AddPolyline(screenQuadPts, 4,
                            GetColorU32(ImGuizmoCol_AxisX + i), true, 1.0f);
      drawList->AddConvexPolyFilled(screenQuadPts, 4, colors[i + 4]);
    }
  }

  const float kCircleRadius{ 6.0f };
  drawList->AddCircleFilled(gContext.gizmoOrigin, kCircleRadius, colors[0], 32);

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    glm::vec2 tail = WorldToScreen(gContext.matrixOrigin,
                                   gContext.camera.viewProjectionMatrix);
    glm::vec2 head = WorldToScreen(gContext.modelMatrix[3],
                                   gContext.camera.viewProjectionMatrix);
    glm::vec2 dif = glm::normalize(head - tail) * (kCircleRadius - 1.0f);

    const auto kTranslationLineColor = 0xAAAAAAAA;
    drawList->AddCircle(tail, kCircleRadius, kTranslationLineColor);
    drawList->AddCircle(head, kCircleRadius, kTranslationLineColor);
    drawList->AddLine(tail + dif, head - dif, kTranslationLineColor, 2.0f);

    const glm::vec3 deltaInfo{ glm::vec3{ gContext.modelMatrix[3] } -
                               gContext.matrixOrigin };
    int componentInfoIndex = (type - ImGuizmoMoveType_MoveX) * 3;
    char tmps[512];
    ImFormatString(tmps, sizeof(tmps),
                   kTranslationInfoMask[type - ImGuizmoMoveType_MoveX],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex]],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex + 1]],
                   deltaInfo[kTranslationInfoIndex[componentInfoIndex + 2]]);

    DrawText(head, tmps);
  }
}
static void DrawRotationGizmo(int type) {
  auto *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, type, ImGuizmoOperation_Rotate);

  glm::vec3 cameraToModelNormalized;
  if (gContext.isOrtho) {
    const auto inversedViewMatrix = glm::inverse(gContext.camera.viewMatrix);
    cameraToModelNormalized = inversedViewMatrix[2];
  } else {
    cameraToModelNormalized = glm::normalize(
      glm::vec3{ gContext.modelMatrix[3] } - gContext.camera.eye);
  }
  // Always face to camera
  cameraToModelNormalized =
    gContext.inversedModelMatrix * glm::vec4{ cameraToModelNormalized, 0.0f };

  gContext.mRadiusSquareCenter = kScreenRotateSize * gContext.viewport.height;

#if 0
  const glm::vec2 gizmoOriginScreenSpace{ WorldToScreen(
    gContext.modelMatrix[3], gContext.camera.viewProjectionMatrix) };
#else
  const glm::vec2 gizmoOriginScreenSpace{ gContext.gizmoOrigin };
#endif

  for (int axis = ImGuizmoAxis_X; axis < ImGuizmoAxis_COUNT; ++axis) {
    const float angleStart{ (glm::atan(
                              cameraToModelNormalized[(4 - axis) % 3],
                              cameraToModelNormalized[(3 - axis) % 3])) +
                            PI * 0.5f };

    ImVec2 circlePos[kArcSegmentCount];
    for (int i = 0; i < kArcSegmentCount; ++i) {
      const float ng{ angleStart +
                      PI * (static_cast<float>(i) / kArcSegmentCount) };
      const glm::vec3 axisPos{ glm::cos(ng), glm::sin(ng), 0.0f };
      const auto pos = glm::vec3{ axisPos[axis], axisPos[(axis + 1) % 3],
                                  axisPos[(axis + 2) % 3] } *
                       gContext.screenFactor;
      circlePos[i] = WorldToScreen(pos, gContext.modelViewProjMatrix);
    }

    const float radiusAxis{ glm::length(gizmoOriginScreenSpace -
                                        glm::vec2{ circlePos[0] }) };
    if (radiusAxis > gContext.mRadiusSquareCenter)
      gContext.mRadiusSquareCenter = radiusAxis;

    drawList->AddPolyline(circlePos, kArcSegmentCount,
                          colors[ImGuizmoAxis_COUNT - axis], false, 2.0f);
  }

  // Circle parallel to view
  drawList->AddCircle(gizmoOriginScreenSpace, gContext.mRadiusSquareCenter,
                      colors[0], 64, 3.0f);

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    ImVec2 circlePos[kArcSegmentCount + 1]{};
    circlePos[0] = gizmoOriginScreenSpace;

    for (auto i = 1; i < kArcSegmentCount; ++i) {
      const float ng{ gContext.rotationAngle *
                      (static_cast<float>(i - 1) / (kArcSegmentCount - 1)) };

      const glm::mat3 rotateVectorMatrix{ glm::rotate(
        glm::mat4{ 1.0f }, ng, glm::vec3{ gContext.translationPlane }) };

      glm::vec3 pos{ rotateVectorMatrix * gContext.rotationVectorSource };
      pos *= gContext.screenFactor;
      circlePos[i] = WorldToScreen(pos + glm::vec3{ gContext.modelMatrix[3] },
                                   gContext.camera.viewProjectionMatrix);
    }

    // Draw inner part ...
    drawList->AddConvexPolyFilled(circlePos, kArcSegmentCount,
                                  GetColorU32(ImGuizmoCol_Selection, 0.541f));
    // ... and outline
    drawList->AddPolyline(circlePos, kArcSegmentCount,
                          GetColorU32(ImGuizmoCol_Selection), true, 2.0f);

    const glm::vec2 destinationPosOnScreen{ circlePos[1] };
    char tmps[512];
    ImFormatString(
      tmps, sizeof(tmps), kRotationInfoMask[type - ImGuizmoMoveType_RotateX],
      (gContext.rotationAngle / PI) * 180.0f, gContext.rotationAngle);

    DrawText(destinationPosOnScreen, tmps);
  }
}
static void DrawScaleGizmo(int type) {
  constexpr auto kCircleRadius = 6.0f; // @todo move to global?

  ImDrawList *drawList{ gContext.drawList };
  if (!drawList) return;

  ImU32 colors[7]{};
  ComputeColors(colors, type, ImGuizmoOperation_Scale);

  glm::vec3 scaleDisplay{ 1.0f };
  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    scaleDisplay = gContext.scale;
  }

  for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
    glm::vec3 dirAxis, dirPlaneX, dirPlaneY;
    bool belowAxisLimit, belowPlaneLimit;
    ComputeTripodAxisAndVisibility(i, dirAxis, dirPlaneX, dirPlaneY,
                                   belowAxisLimit, belowPlaneLimit);
    // Draw xyz axes
    if (belowAxisLimit) {
      const glm::vec2 tail{ WorldToScreen(
        dirAxis * 0.1f * gContext.screenFactor, gContext.modelViewProjMatrix) };
      const glm::vec2 head{ WorldToScreen(dirAxis * gContext.screenFactor,
                                          gContext.modelViewProjMatrix) };
      const glm::vec2 headScaled{ WorldToScreen((dirAxis * scaleDisplay[i]) *
                                                  gContext.screenFactor,
                                                gContext.modelViewProjMatrix) };

      constexpr auto kLineThickness = 3.0f;
      if (gContext.inUse && (gContext.actualID == -1 ||
                             gContext.actualID == gContext.editingID)) {
        drawList->AddLine(tail, head, kDarkGrayColor,
                          kLineThickness);
        drawList->AddCircleFilled(head, kCircleRadius,
                                  kDarkGrayColor);
      }
      drawList->AddLine(tail, headScaled, colors[i + 1], kLineThickness);
      drawList->AddCircleFilled(headScaled, kCircleRadius, colors[i + 1]);

      if (gContext.axisFactor[i] < 0)
        DrawHatchedAxis(dirAxis * scaleDisplay[i]);
    }
  }

  // Draw circle at the begining of axes (center)
  drawList->AddCircleFilled(gContext.gizmoOrigin, kCircleRadius, colors[0], 32);
  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    const glm::vec2 gizmoOriginScreenSpace{ WorldToScreen(
      gContext.modelMatrix[3], gContext.camera.viewProjectionMatrix) };

    char tmps[512];
    const int componentInfoIndex{ (type - ImGuizmoMoveType_ScaleX) * 3 };
    ImFormatString(tmps, sizeof(tmps),
                   kScaleInfoMask[type - ImGuizmoMoveType_ScaleX],
                   scaleDisplay[kTranslationInfoIndex[componentInfoIndex]]);

    DrawText(gizmoOriginScreenSpace, tmps);
  }
}

/**
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [out] type
 * @param [out] snap
 */
static bool HandleTranslation(glm::mat4 &matrix, float *deltaMatrix, int &type,
                              float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };
  bool applyRotationLocaly{ gContext.mode == ImGuizmoMode_Local ||
                            type == ImGuizmoMoveType_MoveScreen };
  bool modified{ false };

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    ImGui::CaptureMouseFromApp();
    const float len{ glm::abs(IntersectRayPlane(gContext.ray.origin,
                                                gContext.ray.direction,
                                                gContext.translationPlane)) };
    const glm::vec3 newPos{ gContext.ray.origin +
                            gContext.ray.direction * len };
    const glm::vec3 newOrigin{ newPos - gContext.relativeOrigin *
                                          gContext.screenFactor };
    glm::vec3 delta{ newOrigin - glm::vec3{ gContext.modelMatrix[3] } };

    // 1 axis constraint
    if (gContext.currentMoveType >= ImGuizmoMoveType_MoveX &&
        gContext.currentMoveType <= ImGuizmoMoveType_MoveZ) {
      const int axisIndex{ gContext.currentMoveType - ImGuizmoMoveType_MoveX };
      const glm::vec3 axisValue{ gContext.modelMatrix[axisIndex] };

      const float lengthOnAxis{ glm::dot(axisValue, delta) };
      delta = axisValue * lengthOnAxis;
    }

    if (snap) {
      glm::vec3 cumulativeDelta{ glm::vec3{ gContext.modelMatrix[3] } + delta -
                                 gContext.matrixOrigin };
      if (applyRotationLocaly) {
        glm::mat4 modelSourceNormalized{ gContext.modelSourceMatrix };
        for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i)
          modelSourceNormalized[i] = glm::normalize(modelSourceNormalized[i]);

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
      delta = gContext.matrixOrigin + cumulativeDelta -
              glm::vec3{ gContext.modelMatrix[3] };
    }

    if (delta != gContext.lastTranslationDelta) modified = true;
    gContext.lastTranslationDelta = delta;

    const glm::mat4 deltaMatrixTranslation{ glm::translate(glm::mat4{ 1.0f },
                                                           delta) };
    if (deltaMatrix) {
      memcpy(deltaMatrix, glm::value_ptr(deltaMatrixTranslation),
             sizeof(glm::mat4));
    }

    matrix = std::move(deltaMatrixTranslation * gContext.modelSourceMatrix);

    if (!io.MouseDown[0]) gContext.inUse = false;
    type = gContext.currentMoveType;
  } else {
    // Find new possible way to move
    glm::vec3 gizmoHitProportion;
    type = GetMoveType(&gizmoHitProportion);
    if (type != ImGuizmoMoveType_None) {
      ImGui::CaptureMouseFromApp();
    }
    if (CanActivate() && type != ImGuizmoMoveType_None) {
      gContext.inUse = true;
      gContext.editingID = gContext.actualID;
      gContext.currentMoveType = type;
      glm::vec3 movePlaneNormal[]{
        gContext.modelMatrix[0],   gContext.modelMatrix[1],
        gContext.modelMatrix[2],   gContext.modelMatrix[0],
        gContext.modelMatrix[1],   gContext.modelMatrix[2],
        -gContext.camera.direction
      };

      const glm::vec3 cameraToModelNormalized{ glm::normalize(
        glm::vec3{ gContext.modelMatrix[3] } - gContext.camera.eye) };
      for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
        const glm::vec3 orthoVector{ glm::cross(movePlaneNormal[i],
                                                cameraToModelNormalized) };
        movePlaneNormal[i] =
          glm::normalize(glm::cross(movePlaneNormal[i], orthoVector));
      }

      gContext.translationPlane =
        BuildPlane(gContext.modelMatrix[3],
                   movePlaneNormal[type - ImGuizmoMoveType_MoveX]);
      const auto len = IntersectRayPlane(
        gContext.ray.origin, gContext.ray.direction, gContext.translationPlane);
      gContext.translationPlaneOrigin =
        gContext.ray.origin + gContext.ray.direction * len;
      gContext.matrixOrigin = gContext.modelMatrix[3];

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
 * @param [out] type
 * @param [out] snap
 */
static bool HandleRotation(glm::mat4 &matrix, float *deltaMatrix, int &type,
                           float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };

  bool applyRotationLocaly{ gContext.mode == ImGuizmoMode_Local };
  bool modified{ false };

  if (!gContext.inUse) {
    type = GetRotateType();
    if (type != ImGuizmoMoveType_None) ImGui::CaptureMouseFromApp();
    if (type == ImGuizmoMoveType_RotateScreen) applyRotationLocaly = true;

    if (CanActivate() && type != ImGuizmoMoveType_None) {
      gContext.inUse = true;
      gContext.editingID = gContext.actualID;
      gContext.currentMoveType = type;

      const glm::vec3 rotatePlaneNormal[]{ gContext.modelMatrix[0],
                                           gContext.modelMatrix[1],
                                           gContext.modelMatrix[2],
                                           -gContext.camera.direction };
      if (applyRotationLocaly) {
        gContext.translationPlane =
          BuildPlane(gContext.modelMatrix[3],
                     rotatePlaneNormal[type - ImGuizmoMoveType_RotateX]);
      } else {
        gContext.translationPlane =
          BuildPlane(gContext.modelSourceMatrix[3],
                     kUnitDirection[type - ImGuizmoMoveType_RotateX]);
      }

      const float len = IntersectRayPlane(
        gContext.ray.origin, gContext.ray.direction, gContext.translationPlane);
      const auto localPos =
        glm::vec3{ gContext.ray.origin + gContext.ray.direction * len -
                   gContext.modelMatrix[3] };
      gContext.rotationVectorSource = glm::normalize(localPos);
      gContext.rotationAngleOrigin = ComputeAngleOnPlane();
    }
  }

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    ImGui::CaptureMouseFromApp();
    gContext.rotationAngle = ComputeAngleOnPlane();
    if (snap) ComputeSnap(&gContext.rotationAngle, glm::radians(snap[0]));

    const glm::vec3 rotationAxisLocalSpace{ glm::normalize(
      glm::mat3{ gContext.inversedModelMatrix } *
      glm::vec3{ gContext.translationPlane }) };

    const glm::mat4 deltaRotation{ glm::rotate(
      glm::mat4{ 1.0f }, gContext.rotationAngle - gContext.rotationAngleOrigin,
      rotationAxisLocalSpace) };

    if (gContext.rotationAngle != gContext.rotationAngleOrigin) modified = true;
    gContext.rotationAngleOrigin = gContext.rotationAngle;

    const glm::mat4 scaleOrigin{ glm::scale(glm::mat4{ 1.0f },
                                            gContext.modelScaleOrigin) };

    if (applyRotationLocaly) {
      matrix = gContext.modelMatrix * deltaRotation * scaleOrigin;
    } else {
      glm::mat4 result{ gContext.modelSourceMatrix };
      result[3] = glm::vec4{ 0.0f };
      matrix = std::move(deltaRotation * result);
      matrix[3] = gContext.modelSourceMatrix[3];
    }

    if (deltaMatrix) {
      memcpy(deltaMatrix,
             glm::value_ptr(gContext.modelMatrix * deltaRotation *
                            gContext.inversedModelMatrix),
             sizeof(glm::mat4));
    }

    if (!io.MouseDown[0]) {
      gContext.inUse = false;
      gContext.editingID = -1;
    }
    type = gContext.currentMoveType;
  }

  return modified;
}

/**
 * @param [out] matrix
 * @param [out] deltaMatrix
 * @param [out] type
 * @param [out] snap
 */
static bool HandleScale(glm::mat4 &matrix, float *deltaMatrix, int &type,
                        float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };
  bool modified{ false };

  if (!gContext.inUse) {
    type = GetScaleType();
    if (type != ImGuizmoMoveType_None) {
      ImGui::CaptureMouseFromApp();
    }
    if (CanActivate() && type != ImGuizmoMoveType_None) {
      gContext.inUse = true;
      gContext.editingID = gContext.actualID;
      gContext.currentMoveType = type;

      const glm::vec3 movePlaneNormal[]{
        gContext.modelMatrix[1],   gContext.modelMatrix[2],
        gContext.modelMatrix[0],   gContext.modelMatrix[2],
        gContext.modelMatrix[1],   gContext.modelMatrix[0],
        -gContext.camera.direction
      };

      gContext.translationPlane =
        BuildPlane(gContext.modelMatrix[3],
                   movePlaneNormal[type - ImGuizmoMoveType_ScaleX]);
      const float len{ IntersectRayPlane(gContext.ray.origin,
                                         gContext.ray.direction,
                                         gContext.translationPlane) };
      gContext.translationPlaneOrigin =
        gContext.ray.origin + gContext.ray.direction * len;
      gContext.matrixOrigin = gContext.modelMatrix[3];
      gContext.scale = glm::vec3{ 1.0f };
      gContext.relativeOrigin = (gContext.translationPlaneOrigin -
                                 glm::vec3{ gContext.modelMatrix[3] }) *
                                (1.0f / gContext.screenFactor);

      #if 1
      for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
        gContext.scaleValueOrigin[i] =
          glm::length(gContext.modelSourceMatrix[i]);
      }
      #else
      gContext.scaleValueOrigin =
        glm::vec3{ glm::length(gContext.modelSourceMatrix[0]),
                   glm::length(gContext.modelSourceMatrix[1]),
                   glm::length(gContext.modelSourceMatrix[2]) };
      #endif
      gContext.mSaveMousePosx = io.MousePos.x;
    }
  }

  if (gContext.inUse &&
      (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
    ImGui::CaptureMouseFromApp();
    const float len{ IntersectRayPlane(
      gContext.ray.origin, gContext.ray.direction, gContext.translationPlane) };
    const glm::vec3 newPos{ gContext.ray.origin +
                            gContext.ray.direction * len };
    const glm::vec3 newOrigin{ newPos - gContext.relativeOrigin *
                                          gContext.screenFactor };
    glm::vec3 delta{ newOrigin - glm::vec3{ gContext.modelMatrix[3] } };

    // 1 axis constraint
    if (gContext.currentMoveType >= ImGuizmoMoveType_ScaleX &&
        gContext.currentMoveType <= ImGuizmoMoveType_ScaleZ) {
      const int axisIndex{ gContext.currentMoveType - ImGuizmoMoveType_ScaleX };
      const glm::vec3 axisValue{ gContext.modelMatrix[axisIndex] };
      const float lengthOnAxis{ glm::dot(axisValue, delta) };
      delta = axisValue * lengthOnAxis;

      const glm::vec3 baseVector{ gContext.translationPlaneOrigin -
                                  glm::vec3{ gContext.modelMatrix[3] } };
      const float ratio{ glm::dot(axisValue, baseVector + delta) /
                         glm::dot(axisValue, baseVector) };

      gContext.scale[axisIndex] = glm::max(ratio, 0.001f);
    } else {
      const float scaleDelta{ (io.MousePos.x - gContext.mSaveMousePosx) *
                              0.01f };
      gContext.scale = glm::vec3{ glm::max(1.0f + scaleDelta, 0.001f) };
    }

    if (snap) {
      const float scaleSnap[]{ snap[0], snap[0], snap[0] };
      ComputeSnap(gContext.scale, scaleSnap);
    }

    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i)
      gContext.scale[i] = glm::max(gContext.scale[i], 0.001f);

    if (gContext.lastScale != gContext.scale) modified = true;
    gContext.lastScale = gContext.scale;

    glm::mat4 deltaMatrixScale{ glm::scale(
      glm::mat4{ 1.0f }, gContext.scale * gContext.scaleValueOrigin) };

    matrix = std::move(gContext.modelMatrix * deltaMatrixScale);

    if (deltaMatrix) {
      deltaMatrixScale = glm::scale(glm::mat4{ 1.0f }, gContext.scale);
      memcpy(deltaMatrix, glm::value_ptr(deltaMatrixScale), sizeof(glm::mat4));
    }

    if (!io.MouseDown[0]) gContext.inUse = false;
    type = gContext.currentMoveType;
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
    for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; i++) {
      const glm::vec3 dirPlaneNormalWorld{ glm::normalize(
        gContext.modelSourceMatrix *
        glm::vec4{ glm::vec3{ kUnitDirection[i] }, 0.0f }) };

      const float dt{ glm::abs(
        glm::dot(glm::normalize(gContext.camera.eye -
                                glm::vec3{ gContext.modelSourceMatrix[3] }),
                 dirPlaneNormalWorld)) };

      if (dt >= bestDot) {
        bestDot = dt;
        bestAxis = i;
        bestAxisWorldDirection = dirPlaneNormalWorld;
      }

      if (dt >= 0.1f) {
        axes[numAxes] = i;
        axesWorldDirections[numAxes] = dirPlaneNormalWorld;
        ++numAxes;
      }
    }
  }

  if (numAxes == 0) {
    axes[0] = bestAxis;
    axesWorldDirections[0] = bestAxisWorldDirection;
    numAxes = 1;
  }

  else if (bestAxis != axes[0]) {
    unsigned int bestIndex{ 0 };
    for (unsigned int i = 0; i < numAxes; i++) {
      if (axes[i] == bestAxis) {
        bestIndex = i;
        break;
      }
    }
    int tempAxis{ axes[0] };
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
      glm::vec2 worldBound1 = WorldToScreen(aabb[i], boundsMVP);
      glm::vec2 worldBound2 = WorldToScreen(aabb[(i + 1) % 4], boundsMVP);
      if (!IsInContextRect(worldBound1) || !IsInContextRect(worldBound2))
        continue;

      float boundDistance = sqrtf(ImLengthSqr(worldBound1 - worldBound2));
      int stepCount = (int)(boundDistance / 10.0f);
      stepCount = glm::min(stepCount, 1000);
      float stepLength = 1.0f / (float)stepCount;
      for (int j = 0; j < stepCount; j++) {
        float t1 = (float)j * stepLength;
        float t2 = (float)j * stepLength + stepLength * 0.5f;
        ImVec2 worldBoundSS1 = ImLerp(worldBound1, worldBound2, ImVec2(t1, t1));
        ImVec2 worldBoundSS2 = ImLerp(worldBound1, worldBound2, ImVec2(t2, t2));
        // drawList->AddLine(worldBoundSS1, worldBoundSS2, 0x000000 +
        // anchorAlpha, 3.0f);
        drawList->AddLine(worldBoundSS1, worldBoundSS2, 0xAAAAAA + anchorAlpha,
                          2.0f);
      }
      glm::vec3 midPoint = (aabb[i] + aabb[(i + 1) % 4]) * 0.5f;
      glm::vec2 midBound = WorldToScreen(midPoint, boundsMVP);
      static const float AnchorBigRadius = 8.0f;
      static const float AnchorSmallRadius = 6.0f;
      bool overBigAnchor = ImLengthSqr(worldBound1 - io.MousePos) <=
                           (AnchorBigRadius * AnchorBigRadius);
      bool overSmallAnchor = ImLengthSqr(midBound - io.MousePos) <=
                             (AnchorBigRadius * AnchorBigRadius);

      glm::vec3 gizmoHitProportion;
      int type = ImGuizmoMoveType_None;
      switch (operation) {
      case ImGuizmoOperation_Translate:
        type = GetMoveType(&gizmoHitProportion);
        break;
      case ImGuizmoOperation_Rotate:
        type = GetRotateType();
        break;
      case ImGuizmoOperation_Scale:
        type = GetScaleType();
        break;

      case ImGuizmoOperation_Bounds:
        break;
      }
      if (type != ImGuizmoMoveType_None) {
        overBigAnchor = false;
        overSmallAnchor = false;
      }

      auto kSelectionColor = 0x8A1080FF;

      unsigned int bigAnchorColor =
        overBigAnchor ? kSelectionColor : (0xAAAAAA + anchorAlpha);
      unsigned int smallAnchorColor =
        overSmallAnchor ? kSelectionColor : (0xAAAAAA + anchorAlpha);

      drawList->AddCircleFilled(worldBound1, AnchorBigRadius, 0xFF000000);
      drawList->AddCircleFilled(worldBound1, AnchorBigRadius - 1.2f,
                                bigAnchorColor);

      drawList->AddCircleFilled(midBound, AnchorSmallRadius, 0xFF000000);
      drawList->AddCircleFilled(midBound, AnchorSmallRadius - 1.2f,
                                smallAnchorColor);
      int oppositeIndex = (i + 2) % 4;
      // big anchor on corners
      if (!gContext.bounds.inUse && gContext.enabled && overBigAnchor &&
          CanActivate()) {

        gContext.bounds.pivot = glm::mat4{ gContext.modelSourceMatrix } *
                                glm::vec4{ aabb[(i + 2) % 4], 1.0f };

        gContext.bounds.anchor =
          glm::mat4{ gContext.modelSourceMatrix } * glm::vec4{ aabb[i], 1.0f };

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
        gContext.editingID = gContext.actualID;
        gContext.bounds.matrix = gContext.modelSourceMatrix;
      }
      // small anchor on middle of segment
      if (!gContext.bounds.inUse && gContext.enabled && overSmallAnchor &&
          CanActivate()) {
        glm::vec3 midPointOpposite =
          (aabb[(i + 2) % 4] + aabb[(i + 3) % 4]) * 0.5f;
        gContext.bounds.pivot = glm::mat4{ gContext.modelSourceMatrix } *
                                glm::vec4{ midPointOpposite, 1.0f };
        gContext.bounds.anchor =
          glm::mat4{ gContext.modelSourceMatrix } * glm::vec4{ midPoint, 1.0f };

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
        gContext.editingID = gContext.actualID;
        gContext.bounds.matrix = gContext.modelSourceMatrix;
      }
    }

    if (gContext.bounds.inUse &&
        (gContext.actualID == -1 || gContext.actualID == gContext.editingID)) {
      // matrix_t scale;
      // scale.SetToIdentity();
      glm::mat4 scale{ 1.0f };

      // compute projected mouse position on plan
      const float len = IntersectRayPlane(
        gContext.ray.origin, gContext.ray.direction, gContext.bounds.plane);
      glm::vec3 newPos = gContext.ray.origin + gContext.ray.direction * len;

      // compute a reference and delta vectors base on mouse move
      glm::vec3 deltaVector =
        glm::abs(newPos - glm::vec3{ gContext.bounds.pivot });
      glm::vec3 referenceVector = glm::abs(glm::vec3{ gContext.bounds.anchor } -
                                           glm::vec3{ gContext.bounds.pivot });

      // for 1 or 2 axes, compute a ratio that's used for scale and snap it
      // based on resulting length
      for (int i = 0; i < 2; i++) {
        int axisIndex1 = gContext.bounds.axis[i];
        if (axisIndex1 == -1) {
          continue;
        }

        float ratioAxis = 1.0f;
        glm::vec3 axisDir = glm::abs(gContext.bounds.matrix[axisIndex1]);

        float dtAxis = glm::dot(axisDir, referenceVector);
        float boundSize = bounds[axisIndex1 + 3] - bounds[axisIndex1];
        if (dtAxis > FLT_EPSILON) {
          ratioAxis = glm::dot(axisDir, deltaVector) / dtAxis;
        }

        if (snapValues) {
          float length = boundSize * ratioAxis;
          ComputeSnap(&length, snapValues[axisIndex1]);
          if (boundSize > FLT_EPSILON) {
            ratioAxis = length / boundSize;
          }
          // printf("\rsnap values [%f, %f, %f]", snapValues[0], snapValues[1],
          //      snapValues[2]);
        }
        // printf("\rratio axis = %f", ratioAxis);
        // printf("\rbound size = %f", boundSize);
        // scale.component[axisIndex1] *= ratioAxis;
        scale[axisIndex1] *= ratioAxis;
      }

      // transform matrix
      /*
      matrix_t preScale, postScale;
      preScale.Translation(-gContext.bounds.localPivot);
      postScale.Translation(gContext.bounds.localPivot);
      matrix_t res = preScale * scale * postScale * gContext.bounds.matrix;
      *matrix = res;
      */
      auto preScale = glm::translate(glm::mat4{ 1.0f },
                                     -glm::vec3{ gContext.bounds.localPivot });
      auto postScale = glm::translate(glm::mat4{ 1.0f },
                                      glm::vec3{ gContext.bounds.localPivot });
      auto res =
        glm::mat4{ gContext.bounds.matrix } * postScale * scale * preScale;
      // memcpy(matrix->m, glm::value_ptr(res), sizeof(glm::mat4));
      matrix = res;

      // info text
      char tmps[512];
      ImVec2 destinationPosOnScreen = WorldToScreen(
        gContext.modelMatrix[3], gContext.camera.viewProjectionMatrix);
      ImFormatString(
        tmps, sizeof(tmps), "X: %.2f Y: %.2f Z:%.2f",
        (bounds[3] - bounds[0]) * glm::length(gContext.bounds.matrix[0]) *
          glm::length(scale[0]),
        (bounds[4] - bounds[1]) * glm::length(gContext.bounds.matrix[1]) *
          glm::length(scale[1]),
        (bounds[5] - bounds[2]) * glm::length(gContext.bounds.matrix[2]) *
          glm::length(scale[2]));
      drawList->AddText(
        ImVec2(destinationPosOnScreen.x + 15, destinationPosOnScreen.y + 15),
        0xFF000000, tmps);
      drawList->AddText(
        ImVec2(destinationPosOnScreen.x + 14, destinationPosOnScreen.y + 14),
        0xFFFFFFFF, tmps);
    }

    if (!io.MouseDown[0]) {
      gContext.bounds.inUse = false;
      gContext.editingID = -1;
    }
    if (gContext.bounds.inUse) {
      break;
    }
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

void DecomposeMatrix(const float *matrix, float *translation, float *rotation,
                     float *scale) {
  assert(matrix);

#if 1
  auto mat = *reinterpret_cast<const glm::mat4 *>(matrix);
#else
  auto mat = glm::make_mat4(matrix);
#endif
  for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
    if (scale) scale[i] = glm::length(mat[i]);
    mat[i] = glm::normalize(mat[i]);
  }

  if (rotation) {
    rotation[0] = glm::degrees(glm::atan(mat[1][2], mat[2][2]));
    rotation[1] = glm::degrees(glm::atan(
      -mat[0][2], glm::sqrt(mat[1][2] * mat[1][2] + mat[2][2] * mat[2][2])));
    rotation[2] = glm::degrees(glm::atan(mat[0][1], mat[0][0]));
  }
  if (translation) {
    translation[0] = mat[3].x;
    translation[1] = mat[3].y;
    translation[2] = mat[3].z;
  }
}

void RecomposeMatrix(const float *translation, const float *rotation,
                     const float *scale, float *matrix) {
  assert(matrix && translation && rotation && scale);

  glm::mat4 mat{ 1.0f };
  for (int i = 2; i >= 0; --i) {
    mat *= glm::rotate(glm::mat4{ 1.0f }, glm::radians(rotation[i]),
                       kUnitDirection[i]);
  }
  for (int i = ImGuizmoAxis_X; i < ImGuizmoAxis_COUNT; ++i) {
    const auto validScale =
      glm::abs(scale[i]) < FLT_EPSILON ? 0.001f : scale[i];
    mat[i] *= validScale;
  }
  mat[3] = glm::vec4{ glm::make_vec3(translation), 1.0f };

#if 1
  *reinterpret_cast<glm::mat4 *>(matrix) = std::move(mat);
#else
  memcpy(matrix, glm::value_ptr(mat), sizeof(glm::mat4));
#endif
}

void SetViewport(float x, float y, float width, float height) {
  gContext.viewport.x = x;
  gContext.viewport.y = y;
  gContext.viewport.width = width;
  gContext.viewport.height = height;

  gContext.mXMax = gContext.viewport.x + gContext.viewport.width;
  gContext.mYMax = gContext.viewport.y + gContext.viewport.height;

  gContext.aspectRatio = width / height;
}

void SetOrthographic(bool isOrthographic) { gContext.isOrtho = isOrthographic; }

ImGuizmoStyle &GetStyle() { return gContext.style; }

void SetDrawlist(ImDrawList *drawList) {
  assert(drawList != nullptr);
  gContext.drawList = drawList ? drawList : ImGui::GetWindowDrawList();
}

void PrintContext() {
  ImGui::Begin("ImGuizmo::Debug");

  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  ImGui::Checkbox("InUse", &gContext.inUse);
  ImGui::PopItemFlag();

  ImGui::Text("CurrentMoveType: %s",
              priv::to_str(ImGuizmoMoveType_(gContext.currentMoveType)));
  ImGui::Text("Operation: %s", priv::to_str(gContext.operation));

  ImGui::Text("ID, actual = %d, editing: %d", gContext.actualID,
              gContext.editingID);

  ImGui::Text("GizmoOrigin (screen space): [%0.2f, %0.2f]",
              gContext.gizmoOrigin.x, gContext.gizmoOrigin.y);

  if (ImGui::TreeNode("Camera")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::InputFloat3("Eye", &gContext.camera.eye[0], "%.2f");
    ImGui::InputFloat3("Direction", &gContext.camera.direction[0], "%.2f");
    ImGui::InputFloat3("Up", &gContext.camera.up[0], "%.2f");
    ImGui::InputFloat3("Right", &gContext.camera.right[0], "%.2f");

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

    ImGui::InputFloat3("matrixOrigin", &gContext.matrixOrigin[0]);
    ImGui::InputFloat3("lastTranslationDelta",
                       &gContext.lastTranslationDelta[0]);
    ImGui::InputFloat3("relativeOrigin", &gContext.relativeOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("Rotation")) {
    ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

    ImGui::Text("Radius center: %f", gContext.mRadiusSquareCenter);

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
    ImGui::InputFloat3("scale", &gContext.scale[0]);
    ImGui::InputFloat3("lastScale", &gContext.lastScale[0]);
    ImGui::InputFloat3("scaleValueOrigin", &gContext.scaleValueOrigin[0]);

    ImGui::PopItemFlag();
    ImGui::TreePop();
  }

  ImGui::End();
}

void BeginFrame() {
  const ImGuiIO &io{ ImGui::GetIO() };

  PrintContext();

#ifdef IMGUI_HAS_VIEWPORT
  ImGui::SetNextWindowSize(ImGui::GetMainViewport()->Size);
  ImGui::SetNextWindowPos(ImGui::GetMainViewport()->Pos);
#else
  ImGui::SetNextWindowSize(io.DisplaySize);
  ImGui::SetNextWindowPos(ImVec2(0, 0));
#endif

  ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
  ImGui::PushStyleColor(ImGuiCol_Border, 0);
  ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

  const auto flags = ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoResize |
                     ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoInputs |
                     ImGuiWindowFlags_NoSavedSettings |
                     ImGuiWindowFlags_NoFocusOnAppearing |
                     ImGuiWindowFlags_NoBringToFrontOnFocus;

  ImGui::Begin("ImGuizmo", nullptr, flags);
  gContext.drawList = ImGui::GetWindowDrawList();
  ImGui::End();
  ImGui::PopStyleVar();
  ImGui::PopStyleColor(2);
}

bool IsUsing() { return gContext.inUse || gContext.bounds.inUse; }

bool IsOver() {
  return (gContext.operation == ImGuizmoOperation_Translate &&
          priv::GetMoveType(nullptr) != ImGuizmoMoveType_None) ||
         (gContext.operation == ImGuizmoOperation_Rotate &&
          priv::GetRotateType() != ImGuizmoMoveType_None) ||
         (gContext.operation == ImGuizmoOperation_Scale &&
          priv::GetScaleType() != ImGuizmoMoveType_None) ||
         IsUsing();
}

bool IsOver(ImGuizmoOperation_ op) {
  switch (op) {
  case ImGuizmoOperation_Scale:
    return priv::GetScaleType() != ImGuizmoMoveType_None || IsUsing();
  case ImGuizmoOperation_Rotate:
    return priv::GetRotateType() != ImGuizmoMoveType_None || IsUsing();
  case ImGuizmoOperation_Translate:
    return priv::GetMoveType(nullptr) != ImGuizmoMoveType_None || IsUsing();
  }
  return false;
}

void Enable(bool enabled) {
  gContext.enabled = enabled;
  if (!enabled) {
    gContext.inUse = false;
    gContext.bounds.inUse = false;
  }
}

void SetID(int id) { gContext.actualID = id; }

/**
 * @todo What is delta matrix?
 *
 * @param [in] view
 * @param [in] projection
 * @param [out] model
 * @param [out](optional) deltaMatrix 
 * @param [out] snap
 * @param [in] localBounds
 * @param [in] boundsSnap
 */
bool Manipulate(const float *view, const float *projection,
                ImGuizmoOperation_ operation, ImGuizmoMode_ mode, float *model,
                float *deltaMatrix, float *snap, const float *localBounds,
                const float *boundsSnap) {
  assert(view && projection && model);

  if (!ImGui::GetIO().WantCaptureMouse)
    gContext.currentMoveType = ImGuizmoMoveType_None;

  #if 0
  priv::ComputeContext(glm::make_mat4(view), glm::make_mat4(projection),
                       glm::make_mat4(model), mode);
  #else
  priv::ComputeContext(*reinterpret_cast<const glm::mat4 *>(view),
                       *reinterpret_cast<const glm::mat4 *>(projection),
                       *reinterpret_cast<const glm::mat4 *>(model), mode);
  #endif

  if (deltaMatrix)
    memcpy(deltaMatrix, glm::value_ptr(glm::mat4{ 1.0f }), sizeof(glm::mat4));

  const glm::vec3 camSpacePosition{ gContext.modelViewProjMatrix *
                                    glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  
  if (!gContext.isOrtho && camSpacePosition.z < 0.001f)
    return false; // Behind camera

  int type{ ImGuizmoMoveType_None };
  bool manipulated{ false };
  if (gContext.enabled) {
    if (!gContext.bounds.inUse) {

       // Prevents operation switch while using
      if (gContext.inUse && gContext.operation != operation)
        operation = gContext.operation;

      switch (operation) {
      case ImGuizmoOperation_Rotate:
        manipulated = priv::HandleRotation(
          *reinterpret_cast<glm::mat4 *>(model), deltaMatrix, type, snap);
        break;
      case ImGuizmoOperation_Translate:
        manipulated = priv::HandleTranslation(
          *reinterpret_cast<glm::mat4 *>(model), deltaMatrix, type, snap);
        break;
      case ImGuizmoOperation_Scale:
        manipulated = priv::HandleScale(*reinterpret_cast<glm::mat4 *>(model),
                                        deltaMatrix, type, snap);
        break;

      case ImGuizmoOperation_Bounds:
        break;
      }
    }
  }

  if (localBounds && !gContext.inUse) {
    priv::HandleAndDrawLocalBounds(localBounds,
                                   *reinterpret_cast<glm::mat4 *>(model),
                                   boundsSnap, operation);
  }

  gContext.operation = operation;
  if (!gContext.bounds.inUse) {
    switch (operation) {
    case ImGuizmoOperation_Rotate:
      priv::DrawRotationGizmo(type);
      break;
    case ImGuizmoOperation_Translate:
      priv::DrawTranslationGizmo(type);
      break;
    case ImGuizmoOperation_Scale:
      priv::DrawScaleGizmo(type);
      break;

    case ImGuizmoOperation_Bounds:
      break;
    }
  }

  return manipulated;
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

  glm::vec4 frustum[6];

  const auto viewProjectionMatrix =
    glm::make_mat4(projection) * glm::make_mat4(view);
  priv::ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));
  int cubeFaceCount = 0;
  for (int cube = 0; cube < modelMatrixCount; ++cube) {
    // const float *matrix = &matrices[cube * 16];

    const auto modelMatrix = glm::make_mat4(&modelMatrices[cube * 16]);
    auto modelViewProjMatrix = viewProjectionMatrix * modelMatrix;

    for (int iFace = 0; iFace < 6; ++iFace) {
      const int normalIndex = (iFace % 3);
      const int perpXIndex = (normalIndex + 1) % 3;
      const int perpYIndex = (normalIndex + 2) % 3;
      const float invert = (iFace > 2) ? -1.0f : 1.0f;

      const glm::vec3 faceCoords[4] = {
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] + kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] -
          kUnitDirection[perpYIndex],
        kUnitDirection[normalIndex] - kUnitDirection[perpXIndex] +
          kUnitDirection[perpYIndex],
      };

      glm::vec3 centerPosition = modelMatrix * glm::vec4{
        glm::vec3{ (kUnitDirection[normalIndex] * 0.5f * invert) }, 1.0f
      };
      glm::vec4 centerPositionVP = modelViewProjMatrix * glm::vec4{
        glm::vec3{ (kUnitDirection[normalIndex] * 0.5f * invert) }, 1.0f
      };

      bool inFrustum = true;
      for (int iFrustum = 0; iFrustum < 6; ++iFrustum) {
        float dist = priv::DistanceToPlane(centerPosition, frustum[iFrustum]);
        if (dist < 0) {
          inFrustum = false;
          break;
        }
      }

      if (!inFrustum) continue;

      cubeFace_t &cubeFace = faces[cubeFaceCount];

      // 3D->2D
      for (auto iCoord = 0; iCoord < 4; iCoord++) {
        cubeFace.faceCoordsScreen[iCoord] = priv::WorldToScreen(
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
    const cubeFace_t &cubeFace = faces[iFace];
    gContext.drawList->AddConvexPolyFilled(cubeFace.faceCoordsScreen, 4,
                                           cubeFace.color);
  }
}

void DrawGrid(const float *view, const float *projection, const float *model,
              const float gridSize) {
  const auto viewProjectionMatrix =
    glm::make_mat4(projection) * glm::make_mat4(view);

  glm::vec4 frustum[6];
  priv::ComputeFrustumPlanes(frustum, glm::value_ptr(viewProjectionMatrix));

  auto modelViewProjMatrix = viewProjectionMatrix * glm::make_mat4(model);

  for (float f = -gridSize; f <= gridSize; f += 1.0f) {
    for (int dir = 0; dir < 2; dir++) {
      glm::vec3 ptA =
        glm::vec3{ dir ? -gridSize : f, 0.0f, dir ? f : -gridSize };
      glm::vec3 ptB = glm::vec3{ dir ? gridSize : f, 0.0f, dir ? f : gridSize };
      bool visible = true;
      for (int i = 0; i < 6; i++) {
        float dA = priv::DistanceToPlane(ptA, frustum[i]);
        float dB = priv::DistanceToPlane(ptB, frustum[i]);
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
        ImU32 col = kMediumGrayColor;
        // auto colr = ImGui::ColorConvertU32ToFloat4(0xFF909090);

        col = (fmodf(fabsf(f), 10.0f) < FLT_EPSILON) ? 0xFF909090 : col;
        col = (fabsf(f) < FLT_EPSILON) ? kDarkGrayColor : col;

        float thickness = 1.0f;
        thickness = (fmodf(fabsf(f), 10.0f) < FLT_EPSILON) ? 1.5f : thickness;
        thickness = (fabsf(f) < FLT_EPSILON) ? 2.3f : thickness;

        gContext.drawList->AddLine(
          priv::WorldToScreen(ptA, modelViewProjMatrix),
          priv::WorldToScreen(ptB, modelViewProjMatrix), col, thickness);
      }
    }
  }
}

/**
 * @param [in/out] view
 */
void ViewManipulate(float *view, float length, ImVec2 position, ImVec2 size,
                    ImU32 backgroundColor) {
  const ImGuiIO &io{ ImGui::GetIO() };

  static bool isDraging{ false };
  static bool isClicking{ false };
  static bool isInside{ false };

  static glm::vec3 interpolationUp{ 0.0f };
  static glm::vec3 interpolationDir{ 0.0 };
  static int interpolationFrames{ 0 };

  // --

  const glm::mat4 lastView{ gContext.camera.viewMatrix };
  const glm::mat4 lastProjection{ gContext.camera.projectionMatrix };

  //
  //
  //

  gContext.drawList->AddRectFilled(position, position + size, backgroundColor);

  const glm::mat4 inversedViewMatrix{ glm::inverse(glm::make_mat4(view)) };
  const glm::vec3 cameraTarget{ inversedViewMatrix[3] -
                                inversedViewMatrix[2] * length };

  constexpr float kDistance{ 1.5f };
  const glm::mat4 cubeProjection{ glm::perspective(
    glm::radians(90.0f), size.x / size.y, 0.1f, 100.0f) };

  const glm::vec3 dir{ inversedViewMatrix[2][0], inversedViewMatrix[2][1],
                       inversedViewMatrix[2][2] };
  const glm::vec3 up{ inversedViewMatrix[1][0], inversedViewMatrix[1][1],
                      inversedViewMatrix[1][2] };
  const glm::mat4 cubeView{ glm::lookAt(dir * kDistance, glm::vec3{ 0.0f },
                                        up) };

  gContext.camera.viewMatrix = cubeView;
  gContext.camera.projectionMatrix = cubeProjection;

  priv::ComputeCameraRay(gContext.ray.origin, gContext.ray.direction, position,
                         size);

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

      // Plane local space
      const glm::vec3 n{ kUnitDirection[normalIndex] * invert };
      const glm::vec3 viewSpaceNormal{ glm::vec3{
        glm::normalize(cubeView * glm::vec4{ n, 0.0f }) } };
      const glm::vec3 viewSpacePoint{ cubeView * glm::vec4{ n * 0.5f, 1.0f } };
      const glm::vec4 viewSpaceFacePlane{ priv::BuildPlane(viewSpacePoint,
                                                           viewSpaceNormal) };
      
      if (viewSpaceFacePlane.w > 0) continue; // Back face culling

      const glm::vec4 facePlane{ priv::BuildPlane(n * 0.5f, n) };
      const float len{ priv::IntersectRayPlane(
        gContext.ray.origin, gContext.ray.direction, facePlane) };

      const glm::vec3 posOnPlane{ gContext.ray.origin +
                                  gContext.ray.direction * len -
                                  (glm::vec4{ n, 1.0f } * 0.5f) };

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

        boxes[boxCoordInt] |= insidePanel && (!isDraging);

        // Draw face with lighter color
        if (iPass) {
          gContext.drawList->AddConvexPolyFilled(
            faceCoordsScreen, 4,
            (priv::GetColorU32(ImGuizmoCol_AxisX + normalIndex) | 0xff1f1f1f) |
              (isInside ? 0x080808 : 0));
          if (boxes[boxCoordInt]) {
            gContext.drawList->AddConvexPolyFilled(faceCoordsScreen, 4,
                                                   0x8060A0F0);

            if (!io.MouseDown[0] && !isDraging && isClicking) {
              // Apply new view direction
              const int cx{ boxCoordInt / 9 };
              const int cy{ (boxCoordInt - cx * 9) / 3 };
              const int cz{ boxCoordInt % 3 };
              interpolationDir = glm::normalize(1.0f - glm::vec3{ cx, cy, cz });
              //auto interpDir2 = glm::normalize(glm::vec3{ 1.0f - cx, 1.0f - cy, 1.0f - cz });
              //assert(interpolationDir == interpDir2);

              if (glm::abs(glm::dot(interpolationDir, kReferenceUp)) > 1.0f - 0.01f) {
                glm::vec3 right{ inversedViewMatrix[0] };
                if (glm::abs(right.x) > glm::abs(right.z)) {
                  right.z = 0.0f;
                } else {
                  right.x = 0.0f;
                }
                right = glm::normalize(right);
                interpolationUp = glm::normalize(glm::cross(interpolationDir, right));
              } else {
                interpolationUp = kReferenceUp;
              }
              interpolationFrames = 40;
              isClicking = false;
            }
            if (io.MouseDown[0] && !isDraging) {
              isClicking = true;
            }
          }
        }
      }
    }
  }

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
    memcpy(view, glm::value_ptr(glm::lookAt(newEye, cameraTarget, newUp)),
           sizeof(glm::mat4));
  }
  isInside = ImRect(position, position + size).Contains(io.MousePos);

  if (!isDraging && io.MouseDown[0] && isInside &&
      (glm::abs(io.MouseDelta.x) > 0.0f || glm::abs(io.MouseDelta.y) > 0.0f)) {
    isDraging = true;
    isClicking = false;
  } else if (isDraging && !io.MouseDown[0]) {
    isDraging = false;
  }

  if (isDraging) {
    const auto angles = -glm::vec2{ io.MouseDelta } * 0.01f;
    const glm::mat4 rx{ glm::rotate(glm::mat4{ 1.0f }, angles.x,
                                    kReferenceUp) };
    const glm::mat4 ry{ glm::rotate(glm::mat4{ 1.0f }, angles.y,
                                    glm::vec3{ inversedViewMatrix[0] }) };
    const glm::mat4 roll{ rx * ry };

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
    memcpy(view,
           glm::value_ptr(glm::lookAt(newEye, cameraTarget, kReferenceUp)),
           sizeof(glm::mat4));
  }

  // Restore view/projection because it was used to compute ray
  priv::ComputeContext(lastView, lastProjection, gContext.modelSourceMatrix,
                       gContext.mode);
}

}; // namespace ImGuizmo