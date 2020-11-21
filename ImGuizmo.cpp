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
constexpr int kCircleSegmentCount{ 128 };

//-----------------------------------------------------------------------------
// [SECTION] INTERNAL TYPES
//-----------------------------------------------------------------------------

enum ImGuizmoAxis_ {
  ImGuizmoAxis_X,
  ImGuizmoAxis_Y,
  ImGuizmoAxis_Z,

  ImGuizmoAxis_COUNT
};
using ImGuizmoAxis = unsigned int;

enum ImGuizmoPlane_ {
  ImGuizmoPlane_YZ,
  ImGuizmoPlane_ZX,
  ImGuizmoPlane_XY,

  ImGuizmoPlane_COUNT
};
using ImGuizmoPlane = unsigned int;

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
  ImGuizmoAxisFlags ActiveManipulationFlags{ ImGuizmoAxisFlags_None };
  ImGuizmoAxisFlags LockedAxesFlags{ ImGuizmoAxisFlags_None };

  bool Dirty{ false }; // Set to true on manipulate

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
  void Load(const float *model);
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
  ImGuizmoConfigFlags ConfigFlags{ ImGuizmoConfigFlags_None };

  ImRect Viewport;
  ImGuizmoCamera Camera;
  ImGuizmoRay Ray;
  glm::vec2 DragOrigin{ 0.0f };

  ImVector<ImGuizmoWidget *> Gizmos;
  ImGuiStorage GizmosById;

  ImGuizmoWidget *CurrentGizmo{ nullptr }; // Gizmo in Begin/End scope
  ImGuizmoWidget *ActiveGizmo{ nullptr };  // Currently manipulated gizmo

  //ImGuizmoCage Bounds;

  float *LockedModelMatrix{ nullptr };
  glm::mat4 BackupModelMatrix{ 1.0f }; // For reverting operation

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

ImGuizmoStyle::ImGuizmoStyle() { ImGuizmo::StyleColorsClassic(this); }

namespace ImGuizmo {

//-----------------------------------------------------------------------------
// [SECTION] STYLING
//-----------------------------------------------------------------------------

ImGuizmoStyle &GetStyle() { return GImGuizmo.Style; }
void StyleColorsClassic(ImGuizmoStyle *dst) {
  ImGuizmoStyle *style{ dst ? dst : &ImGuizmo::GetStyle() };
  ImVec4 *colors{ style->Colors };

  colors[ImGuizmoCol_Inactive] = ImVec4{ 0.60f, 0.60f, 0.60f, 0.60f };
  colors[ImGuizmoCol_Hovered] = ImVec4{ 1.00f, 0.501f, 0.062f, 0.54f };

  colors[ImGuizmoCol_SpecialMove] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };

  colors[ImGuizmoCol_Text] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };
  colors[ImGuizmoCol_TextShadow] = ImVec4{ 0.00f, 0.00f, 0.00f, 1.00f };
  
  colors[ImGuizmoCol_AxisX] = ImVec4{ 0.66f, 0.00f, 0.00f, 1.00f };
  colors[ImGuizmoCol_AxisY] = ImVec4{ 0.00f, 0.66f, 0.00f, 1.00f };
  colors[ImGuizmoCol_AxisZ] = ImVec4{ 0.00f, 0.00f, 0.66f, 1.00f };

  colors[ImGuizmoCol_PlaneYZ] = ImVec4{ 0.66f, 0.00f, 0.00f, 0.38f };
  colors[ImGuizmoCol_PlaneZX] = ImVec4{ 0.00f, 0.66f, 0.00f, 0.38f };
  colors[ImGuizmoCol_PlaneXY] = ImVec4{ 0.00f, 0.00f, 0.66f, 0.38f };
}
void StyleColorsBlender(ImGuizmoStyle *dst) {
  ImGuizmoStyle *style{ dst ? dst : &ImGuizmo::GetStyle() };
  ImVec4 *colors{ style->Colors };

  colors[ImGuizmoCol_Inactive] = ImVec4{ 0.600f, 0.600f, 0.600f, 0.600f };
  colors[ImGuizmoCol_Hovered] = ImVec4{ 1.00f, 0.501f, 0.062f, 1.00f };

  colors[ImGuizmoCol_SpecialMove] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };

  colors[ImGuizmoCol_Text] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };
  colors[ImGuizmoCol_TextShadow] = ImVec4{ 0.00f, 0.00f, 0.00f, 1.00f };

  colors[ImGuizmoCol_AxisX] = ImVec4{ 1.0f, 0.2f, 0.321f, 1.0f };
  colors[ImGuizmoCol_AxisY] = ImVec4{ 0.545f, 0.862f, 0.0f, 1.0f };
  colors[ImGuizmoCol_AxisZ] = ImVec4{ 0.156f, 0.564f, 1.0f, 1.0f };

  colors[ImGuizmoCol_PlaneYZ] = ImVec4{ 1.0f, 0.2f, 0.321f, 0.6f };
  colors[ImGuizmoCol_PlaneZX] = ImVec4{ 0.545f, 0.862f, 0.0f, 0.6f };
  colors[ImGuizmoCol_PlaneXY] = ImVec4{ 0.156f, 0.564f, 1.0f, 0.6f };
}
void StyleColorsUnreal(ImGuizmoStyle *dst) {
  ImGuizmoStyle *style{ dst ? dst : &ImGuizmo::GetStyle() };
  ImVec4 *colors{ style->Colors };

  colors[ImGuizmoCol_Inactive] = ImVec4{ 0.700f, 0.700f, 0.700f, 0.700f };
  colors[ImGuizmoCol_Hovered] = ImVec4{ 1.00f, 1.00f, 0.0f, 1.00f };

  colors[ImGuizmoCol_SpecialMove] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };

  colors[ImGuizmoCol_Text] = ImVec4{ 1.00f, 1.00f, 1.00f, 1.00f };
  colors[ImGuizmoCol_TextShadow] = ImVec4{ 0.00f, 0.00f, 0.00f, 1.00f };

  colors[ImGuizmoCol_AxisX] = ImVec4{ 0.1349f, 0.3959f, 0.0f, 1.0f };
  colors[ImGuizmoCol_AxisY] = ImVec4{ 0.0251f, 0.207f, 0.85f, 1.0f };
  colors[ImGuizmoCol_AxisZ] = ImVec4{ 0.594f, 0.0197f, 0.0f, 1.0f };

  colors[ImGuizmoCol_PlaneYZ] = ImVec4{ 0.1349f, 0.3959f, 0.0f, 0.6f };
  colors[ImGuizmoCol_PlaneZX] = ImVec4{ 0.0251f, 0.207f, 0.85f, 0.6f };
  colors[ImGuizmoCol_PlaneXY] = ImVec4{ 0.594f, 0.0197f, 0.0f, 0.6f };

}

void ShowStyleEditor(ImGuizmoStyle *ref) {
  ImGuizmoStyle &style{ ImGuizmo::GetStyle() };
  static ImGuizmoStyle ref_saved_style;

  static bool init = true;
  if (init && ref == nullptr) ref_saved_style = style;
  init = false;
  if (ref == nullptr) ref = &ref_saved_style;

  ImGui::PushItemWidth(ImGui::GetWindowWidth() * 0.50f);

  if (ImGuizmo::ShowStyleSelector("Colors##Selector")) ref_saved_style = style;

  ImGui::DragFloat("Alpha", &style.Alpha, 0.01f, 0.01f, 1.0f);
  ImGui::DragFloat("GizmoScale", &style.GizmoScale, 0.01f, 0.01f, 1.0f);
  ImGui::DragFloat("RingThickness", &style.RotationRingThickness, 0.1f, 0.1f,
                   10.0f);

  static ImGuiColorEditFlags alpha_flags = ImGuiColorEditFlags_AlphaPreviewHalf;
  for (int i = 0; i < ImGuizmoCol_COUNT; ++i) {
    const char *name = ImGuizmo::GetStyleColorName(i);

    ImGui::PushID(i);
    ImGui::ColorEdit4("##color", (float *)&style.Colors[i],
                      ImGuiColorEditFlags_AlphaBar | alpha_flags);
    ImGui::SameLine();
    ImGui::TextUnformatted(name);
    ImGui::PopID();
  }
}

bool ShowStyleSelector(const char *label) {
  static int style_idx = -1;
  if (ImGui::Combo(label, &style_idx, "Classic\0Blender\0Unreal\0")) {
    switch (style_idx) {
    case 0:
      StyleColorsClassic();
      break;
    case 1:
      StyleColorsBlender();
      break;
    case 2:
      StyleColorsUnreal();
      break;
    }
    return true;
  }
  return false;
}

const char *GetStyleColorName(ImGuizmoCol idx) {
  switch (idx) {
  case ImGuizmoCol_Text: return "Text";
  case ImGuizmoCol_TextShadow: return "TextShadow";
  case ImGuizmoCol_Inactive: return "Inactive";
  case ImGuizmoCol_Hovered: return "Hovered";
  case ImGuizmoCol_SpecialMove: return "SpecialMove";
  case ImGuizmoCol_AxisX: return "AxisX";
  case ImGuizmoCol_AxisY: return "AxisY";
  case ImGuizmoCol_AxisZ: return "AxisZ";
  case ImGuizmoCol_PlaneYZ: return "PlaneYZ";
  case ImGuizmoCol_PlaneZX: return "PlaneZX";
  case ImGuizmoCol_PlaneXY: return "PlaneXY";
  }
  IM_ASSERT(0);
  return "Unknown";
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

static glm::vec2 WorldToScreen(const glm::vec3 &world_pos,
                               const glm::mat4 &matrix,
                               const glm::vec2 &position,
                               const glm::vec2 &size) {
  glm::vec4 temp{ matrix * glm::vec4{ world_pos, 1.0f } };
  temp *= 0.5f / temp.w;

  glm::vec2 screen_pos{ temp.xy() + 0.5f };
  screen_pos.y = 1.0f - screen_pos.y;
  screen_pos *= size;
  screen_pos += position;
  return screen_pos;
}
static glm::vec2 WorldToScreen(const glm::vec3 &world_pos,
                               const glm::mat4 &matrix) {
  const ImGuizmoContext &g{ GImGuizmo };
  return WorldToScreen(world_pos, matrix, g.Viewport.GetTL(),
                       g.Viewport.GetSize());
}

static ImGuizmoRay RayCast(const glm::mat4 &view_proj_matrix,
                           const glm::vec2 &position, const glm::vec2 &size) {
  // Convert to NDC
  glm::vec2 mouse_pos{ ImGui::GetIO().MousePos };
  mouse_pos = ((mouse_pos - position) / size) * 2.0f - 1.0f;
  mouse_pos.y *= -1.0f;

  const glm::mat4 inversed_view_proj{ glm::inverse(view_proj_matrix) };
  glm::vec4 ray_start_world_space{ inversed_view_proj *
                                   glm::vec4{ mouse_pos, 0.0f, 1.0f } };
  ray_start_world_space *= 1.0f / ray_start_world_space.w;
  glm::vec4 ray_end_world_space{
    inversed_view_proj * glm::vec4{ mouse_pos, 1.0f - kEpsilon, 1.0f }
  };
  ray_end_world_space *= 1.0f / ray_end_world_space.w;
  return ImGuizmoRay{ ray_start_world_space, ray_end_world_space,
                      glm::normalize(ray_end_world_space -
                                     ray_start_world_space) };
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

  glm::vec2 seg_a{ points[1] - points[0] };
  seg_a.y /= g.GetAspectRatio();
  glm::vec2 seg_b{ points[2] - points[0] };
  seg_b.y /= g.GetAspectRatio();

  const auto seg_a_ortho = glm::normalize(glm::vec2{ -seg_a.y, seg_a.x });
  const float dt{ glm::dot(seg_a_ortho, seg_b) };
  const float surface{ glm::length(seg_a) * glm::abs(dt) };
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

  // Normal is orthogonal to vector, can't intersect
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

  auto start_of_segment = gizmo->ModelViewProjMatrix * glm::vec4{ start, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(start_of_segment.w) > kEpsilon)
    start_of_segment *= 1.0f / start_of_segment.w;

  auto end_of_segment = gizmo->ModelViewProjMatrix * glm::vec4{ end, 1.0f };
  // Check for axis aligned with camera direction
  if (glm::abs(end_of_segment.w) > kEpsilon)
    end_of_segment *= 1.0f / end_of_segment.w;

  glm::vec2 clip_space_axis{ end_of_segment - start_of_segment };
  clip_space_axis.y /= g.GetAspectRatio();
  return glm::length(clip_space_axis);
}

//-----------------------------------------------------------------------------
// [SECTION] UTILITIES (SNAP)
//-----------------------------------------------------------------------------

static void ComputeSnap(float &value, float snap) {
  if (snap <= kEpsilon) return;

  const float modulo{ glm::fmod(value, snap) };
  const float modulo_ratio{ glm::abs(modulo) / snap };
  constexpr float snap_tension{ 0.5f };
  if (modulo_ratio < snap_tension) {
    value -= modulo;
  } else if (modulo_ratio > (1.0f - snap_tension)) {
    value = value - modulo + snap * ((value < 0.0f) ? -1.0f : 1.0f);
  }
}
static void ComputeSnap(glm::vec3 &value, const float *snap) {
  for (int axis = 0; axis < 3; ++axis)
    ComputeSnap(value[axis], snap[axis]);
}

//-----------------------------------------------------------------------------
// [SECTION] UTILITIES
//-----------------------------------------------------------------------------

bool IsAxisIdxValid(ImGuizmoAxis axis_idx) {
  return axis_idx >= 0 && axis_idx < 3;
}
bool HasSingleAxis(ImGuizmoAxisFlags flags) {
  return flags == ImGuizmoAxisFlags_X || flags == ImGuizmoAxisFlags_Y ||
         flags == ImGuizmoAxisFlags_Z;
}

ImGuizmoAxis GetAxisIdx(ImGuizmoAxisFlags flags, bool around) {
  IM_ASSERT(HasSingleAxis(flags));
  switch (flags) {
  case ImGuizmoAxisFlags_X:
    return around ? ImGuizmoAxis_Z : ImGuizmoAxis_X;
  case ImGuizmoAxisFlags_Y:
    return ImGuizmoAxis_Y;
  case ImGuizmoAxisFlags_Z:
    return around ? ImGuizmoAxis_X : ImGuizmoAxis_Z;

  default:
    return -1;
  }
}
ImGuizmoAxisFlags AxisToFlag(ImGuizmoAxis axis_idx, bool around) {
  IM_ASSERT(IsAxisIdxValid(axis_idx));
  switch (axis_idx) {
  case ImGuizmoAxis_X:
    return around ? ImGuizmoAxisFlags_Z : ImGuizmoAxisFlags_X;
  case ImGuizmoAxis_Y:
    return ImGuizmoAxisFlags_Y;
  case ImGuizmoAxis_Z:
    return around ? ImGuizmoAxisFlags_X : ImGuizmoAxisFlags_Z;

  default:
    return -1;
  }
}

ImGuizmoAxis GetAxisAroundIdx(ImGuizmoAxis axis_idx) {
  IM_ASSERT(IsAxisIdxValid(axis_idx));
  switch (axis_idx) {
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
ImGuizmoAxis GetScaleAxisIdx(ImGuizmoAxis axis_idx) {
  IM_ASSERT(IsAxisIdxValid(axis_idx));
  switch (axis_idx) {
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

bool IsPlaneIdxValid(ImGuizmoPlane planeIdx) {
  return planeIdx >= 0 && planeIdx < 3;
}
bool HasPlane(ImGuizmoAxisFlags flags) {
  if (flags == ImGuizmoAxisFlags_ALL) return false;
  return flags == ImGuizmoAxisFlags_YZ || flags == ImGuizmoAxisFlags_ZX ||
         flags == ImGuizmoAxisFlags_XY;
}
ImGuizmoPlane GetPlaneIdx(ImGuizmoAxisFlags flags) {
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
ImGuizmoAxisFlags PlaneToFlags(ImGuizmoPlane plane_idx) {
  IM_ASSERT(IsPlaneIdxValid(plane_idx));
  switch (plane_idx) {
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
// [SECTION] UTILITIES (TEXT)
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
const int kInfoDataIndices[]{
  0, 1, 2, // XYZ
  1, 2, 0, // YZ (0-unused)
  0, 2, 0, // XZ (0-unused)
  0, 1, 0, // XY (0-unused)
};

void RenderTranslationInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags &flags{ gizmo->ActiveManipulationFlags };
  const char *mask{ nullptr };
  int start_idx{ 0 };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ GetAxisIdx(flags, false) };
    mask = kInfoMasks[axis_idx];
    start_idx = axis_idx;
  } else if (HasPlane(flags)) {
    const ImGuizmoPlane planeIdx{ GetPlaneIdx(flags) };
    mask = kInfoMasks[ImGuizmoAxis_COUNT + planeIdx];
    start_idx = ImGuizmoAxis_COUNT + (ImGuizmoPlane_COUNT * planeIdx);
  } else {
    mask = kInfoMasks[ImGuizmoAxis_COUNT + ImGuizmoPlane_COUNT];
  }

  const glm::vec3 delta_info{ gizmo->ModelMatrix[3].xyz() -
                              gizmo->DragTranslationOrigin };

  char info_buffer[128]{};
  ImFormatString(info_buffer, sizeof(info_buffer), mask,
                 delta_info[kInfoDataIndices[start_idx]],
                 delta_info[kInfoDataIndices[start_idx + 1]],
                 delta_info[kInfoDataIndices[start_idx + 2]]);
  RenderText(gizmo->Origin, info_buffer);
}
void RenderRotationInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };
  const char *mask{ nullptr };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ (GetAxisIdx(flags, false)) };
    mask = kInfoMasks[ImGuizmoAxis_COUNT + ImGuizmoPlane_COUNT + 1 + axis_idx];
  } else {
    mask = kInfoMasks[(ImGuizmoAxis_COUNT * 2) + ImGuizmoPlane_COUNT];
  }

  char info_buffer[128]{};
  ImFormatString(info_buffer, sizeof(info_buffer), mask,
                 glm::degrees(gizmo->RotationAngle), gizmo->RotationAngle);
  RenderText(gizmo->Origin, info_buffer);
}
void RenderScaleInfo() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };
  const char *mask{ nullptr };
  int start_idx{ 0 };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ GetAxisIdx(flags, false) };
    mask = kInfoMasks[axis_idx];
    start_idx = axis_idx;
  } else {
    if (glm::all(glm::equal(gizmo->Scale, glm::vec3{ gizmo->Scale.x })))
      mask = kInfoMasks[11];
    else
      mask = kInfoMasks[6];
  }

  char info_buffer[128]{};
  ImFormatString(info_buffer, sizeof(info_buffer), mask,
                 gizmo->Scale[kInfoDataIndices[start_idx]],
                 gizmo->Scale[kInfoDataIndices[start_idx + 1]],
                 gizmo->Scale[kInfoDataIndices[start_idx + 2]]);
  RenderText(gizmo->Origin, info_buffer);
}

void InspectHoverFlags(const char *label, ImGuizmoAxisFlags flags) {
  ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
  ImGui::Text(label);
  ImGui::SameLine();
  ImGui::CheckboxFlags("X", &flags, ImGuizmoAxisFlags_X);
  ImGui::SameLine();
  ImGui::CheckboxFlags("Y", &flags, ImGuizmoAxisFlags_Y);
  ImGui::SameLine();
  ImGui::CheckboxFlags("Z", &flags, ImGuizmoAxisFlags_Z);
  ImGui::PopItemFlag();
}

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

constexpr auto kDarkGrayColor = 0xFF404040;
constexpr auto kMediumGrayColor = 0xFF808080;

//-----------------------------------------------------------------------------
// [SECTION]
//-----------------------------------------------------------------------------

bool GizmoBehavior(ImGuizmoOperation operation, ImGuizmoAxisFlags &hovered,
                   bool *out_held) {
  IM_ASSERT(operation);

  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (g.ActiveGizmo != gizmo && g.ActiveGizmo) {
    hovered = ImGuizmoAxisFlags_None;
  } else {
    if (gizmo->ActiveOperation != operation && gizmo->ActiveOperation) {
      hovered = ImGuizmoAxisFlags_None;
    } else {
      if (gizmo->ActiveManipulationFlags)
        hovered = gizmo->ActiveManipulationFlags;
    }
  }

  bool pressed{ hovered && io.MouseClicked[0] };
  if (pressed) {
    g.ActiveGizmo = gizmo;
    gizmo->ActiveOperation = operation;
    gizmo->ActiveManipulationFlags = hovered;
  }

  bool held{ false };
  if (gizmo->ActiveManipulationFlags == hovered &&
      gizmo->ActiveManipulationFlags) {
    if (io.MouseDown[0]) {
      held = true;
    } else {
      g.ActiveGizmo = nullptr;
      gizmo->ActiveManipulationFlags = ImGuizmoAxisFlags_None;
    }
  }

  if (out_held) *out_held = held;
  return pressed;
}

void SetViewport(const ImRect &viewport) { GImGuizmo.Viewport = viewport; }

//-----------------------------------------------------------------------------
// [SECTION] HOVER QUERY
//-----------------------------------------------------------------------------

bool IsCoreHovered() {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (gizmo->LockedAxesFlags == ImGuizmoAxisFlags_ALL) return false;

  if constexpr (true) { // Origin as circle
    const float distance{ glm::length(glm::vec2{ io.MousePos } -
                                      gizmo->Origin) };
    return distance <= kCircleRadius;
  } else { // square
    ImRect bb{ gizmo->Origin - kCircleRadius, gizmo->Origin + kCircleRadius };
    return bb.Contains(io.MousePos);
  }
}
bool IsAxisHovered(ImGuizmoAxis axis_idx) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (gizmo->LockedAxesFlags & AxisToFlag(axis_idx, false)) return false;

  const glm::vec3 dir_axis{ gizmo->ModelMatrix *
                            glm::vec4{ kUnitDirections[axis_idx], 0.0f } };
  const float length{ IntersectRayPlane(
    g.Ray, BuildPlane(gizmo->ModelMatrix[3], dir_axis)) };
  const glm::vec2 mouse_pos_on_plane{ WorldToScreen(
    g.Ray.Start + g.Ray.Direction * length, g.Camera.ViewProjectionMatrix) };

  constexpr float axis_shift{ 0.1f };
  const glm::vec2 axis_start_on_screen{ WorldToScreen(
    gizmo->ModelMatrix[3].xyz() + dir_axis * gizmo->ScreenFactor * axis_shift,
    g.Camera.ViewProjectionMatrix) };
  const glm::vec2 axis_end_on_screen{ WorldToScreen(
    gizmo->ModelMatrix[3].xyz() + dir_axis * gizmo->ScreenFactor,
    g.Camera.ViewProjectionMatrix) };
  const glm::vec2 closest_point_on_axis{ PointOnSegment(
    mouse_pos_on_plane, axis_start_on_screen, axis_end_on_screen) };
  constexpr float tolerance{ 6.0f };
  return glm::length(closest_point_on_axis - mouse_pos_on_plane) < tolerance;
}
bool IsPlaneHovered(ImGuizmoPlane plane_idx) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (gizmo->LockedAxesFlags & PlaneToFlags(plane_idx)) return false;

  const glm::vec3 dir_axis{ gizmo->ModelMatrix *
                            glm::vec4{ kUnitDirections[plane_idx], 0.0f } };
  const float length{ IntersectRayPlane(
    g.Ray, BuildPlane(gizmo->ModelMatrix[3], dir_axis)) };
  const glm::vec3 mouse_pos_on_plane{ g.Ray.Start + g.Ray.Direction * length };

  const glm::vec3 plane_dir1{
    gizmo->ModelMatrix * glm::vec4{ kUnitDirections[(plane_idx + 1) % 3], 0.0f }
  };
  const float dx{ glm::dot(plane_dir1,
                           (mouse_pos_on_plane - gizmo->ModelMatrix[3].xyz()) *
                             (1.0f / gizmo->ScreenFactor)) };
  const glm::vec3 plane_dir2{
    gizmo->ModelMatrix * glm::vec4{ kUnitDirections[(plane_idx + 2) % 3], 0.0f }
  };
  const float dy{ glm::dot(plane_dir2,
                           (mouse_pos_on_plane - gizmo->ModelMatrix[3].xyz()) *
                             (1.0f / gizmo->ScreenFactor)) };
  return (dx >= kUnitQuad[0] && dx <= kUnitQuad[4] && dy >= kUnitQuad[1] &&
          dy <= kUnitQuad[3]);
}
bool IsRotationAxisHovered(ImGuizmoAxis axis_idx) {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (gizmo->LockedAxesFlags == AxisToFlag(axis_idx, false)) return false;

  const glm::vec4 pickup_plane{ BuildPlane(gizmo->ModelMatrix[3],
                                           gizmo->ModelMatrix[axis_idx]) };
  const float length{ IntersectRayPlane(g.Ray, pickup_plane) };
  const glm::vec3 local_pos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - gizmo->ModelMatrix[3].xyz()) };

  // ... ?
  if (glm::dot(local_pos, g.Ray.Direction) > kEpsilon) return false;

  const glm::vec3 ideal_pos_on_circle{ glm::mat3{ gizmo->InversedModelMatrix } *
                                       local_pos };
  const glm::vec2 ideal_pos_on_circle_screen_space{ WorldToScreen(
    ideal_pos_on_circle * gizmo->ScreenFactor, gizmo->ModelViewProjMatrix) };

  constexpr float tolerance{ 8.0f };
  const glm::vec2 distance_on_screen{ ideal_pos_on_circle_screen_space -
                                      io.MousePos };
  return glm::length(distance_on_screen) < tolerance;
}
bool IsRotationRingHovered() {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoStyle &style{ GetStyle() };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  if (gizmo->LockedAxesFlags == ImGuizmoAxisFlags_ALL) return false;

  constexpr float tolerance{ 1.0f };
  const float ring_thickness{ style.RotationRingThickness + tolerance };
  const float distance{ glm::length(glm::vec2{ io.MousePos } - gizmo->Origin) };
  return (distance >= gizmo->RingRadius - ring_thickness) &&
         (distance < gizmo->RingRadius + ring_thickness);
}

//-----------------------------------------------------------------------------
// [SECTION] TRANSLATION
//-----------------------------------------------------------------------------

ImGuizmoAxisFlags FindTranslationHover() {
  ImGuizmoAxisFlags hover_flags{ ImGuizmoAxisFlags_None };
  if (IsCoreHovered()) hover_flags |= ImGuizmoAxisFlags_ALL;
  if (hover_flags != ImGuizmoAxisFlags_ALL) {
    for (int plane = 0; plane < 3; ++plane)
      if (IsPlaneHovered(plane)) {
        hover_flags |= PlaneToFlags(plane);
        break;
      }
    if (!HasPlane(hover_flags)) {
      for (int axis = 0; axis < 3; ++axis)
        if (IsAxisHovered(axis)) {
          hover_flags |= AxisToFlag(axis, false);
          break;
        }
    }
  }
  return hover_flags;
}

glm::vec4 BuildTranslatePlane() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };

  glm::vec3 move_plane_normal;
  if (HasPlane(flags)) {
    move_plane_normal = gizmo->ModelMatrix[GetPlaneIdx(flags)];
  } else if (HasSingleAxis(flags)) {
    const glm::vec3 dir{ gizmo->ModelMatrix[GetAxisIdx(flags, false)] };
    const glm::vec3 camera_to_model_normalized{ glm::normalize(
      gizmo->ModelMatrix[3].xyz() - g.Camera.Eye) };
    const glm::vec3 orthoDir{ glm::cross(dir, camera_to_model_normalized) };
    move_plane_normal = glm::normalize(glm::cross(dir, orthoDir));
  } else { // special movement
    move_plane_normal = -g.Camera.Forward;
  }
  return BuildPlane(gizmo->ModelMatrix[3], move_plane_normal);
}
void BeginTranslation() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  g.BackupModelMatrix = gizmo->SourceModelMatrix;
  g.DragOrigin = io.MousePos;

  gizmo->DragTranslationOrigin = gizmo->ModelMatrix[3];
  gizmo->TranslationPlane = BuildTranslatePlane();
  const auto length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
  gizmo->ModelRelativeOrigin =
    (gizmo->TranslationPlaneOrigin - gizmo->ModelMatrix[3].xyz()) *
    (1.0f / gizmo->ScreenFactor);
}
void ContinueTranslation(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };

  const float length{ glm::abs(
    IntersectRayPlane(g.Ray, gizmo->TranslationPlane)) };
  const glm::vec3 target_position{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 new_position{ target_position - gizmo->ModelRelativeOrigin *
                                                    gizmo->ScreenFactor };

  glm::vec3 delta{ new_position - gizmo->ModelMatrix[3].xyz };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ GetAxisIdx(flags, false) };
    const glm::vec3 axis_value{ gizmo->ModelMatrix[axis_idx] };
    ImGui::Text("Axis value = %.2f, %.2f, %.2f", axis_value.x, axis_value.y,
                axis_value.z);

    const float length_on_axis{ glm::dot(axis_value, delta) };
    delta = axis_value * length_on_axis;
  }

  if (snap) {
    glm::vec3 cumulative_delta{ gizmo->ModelMatrix[3].xyz + delta -
                                gizmo->DragTranslationOrigin };
    bool apply_rotation_localy{ gizmo->Mode == ImGuizmoMode_Local ||
                                flags == ImGuizmoAxisFlags_ALL };
    if (apply_rotation_localy) {
      glm::mat4 source_model_normalized{ gizmo->SourceModelMatrix };
      for (int axis = 0; axis < 3; ++axis)
        source_model_normalized[axis] =
          glm::normalize(source_model_normalized[axis]);
      cumulative_delta = glm::inverse(source_model_normalized) *
                         glm::vec4{ cumulative_delta, 0.0f };
      ComputeSnap(cumulative_delta, snap);
      cumulative_delta =
        source_model_normalized * glm::vec4{ cumulative_delta, 0.0f };
    } else {
      ComputeSnap(cumulative_delta, snap);
    }
    delta = gizmo->DragTranslationOrigin + cumulative_delta -
            gizmo->ModelMatrix[3].xyz;
  }

  if (delta != gizmo->LastTranslationDelta) {
    gizmo->ModelMatrix = glm::translate(delta) * gizmo->SourceModelMatrix;
    // Handle locked axes
    for (int axis = 0; axis < 3; ++axis)
      if (gizmo->LockedAxesFlags & AxisToFlag(axis, false))
        gizmo->ModelMatrix[3][axis] = gizmo->DragTranslationOrigin[axis];
    gizmo->Dirty = true;
  }
  gizmo->LastTranslationDelta = delta;
}

void RenderAxis(ImGuizmoAxis axis_idx, ImGuizmoAxisFlags flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_AxisX + axis_idx) };
  if (gizmo->LockedAxesFlags & AxisToFlag(axis_idx, false)) {
    if (g.ConfigFlags & ImGuizmoConfigFlags_HideLocked) return;
    color = GetColorU32(ImGuizmoCol_Inactive);
  } else if (HasSingleAxis(flags) && GetAxisIdx(flags, false) == axis_idx) {
    color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);
  }

  const glm::vec3 &dir_axis{ kUnitDirections[axis_idx] };

  constexpr float visibility_threshold{ 0.03f };
  const float axis_length{ GetSegmentLengthClipSpace(
    glm::vec3{ 0.0f }, dir_axis * gizmo->ScreenFactor) };
  if (axis_length < visibility_threshold) return;

  const glm::vec2 tail{ WorldToScreen(dir_axis * 0.1f * gizmo->ScreenFactor,
                                      gizmo->ModelViewProjMatrix) };
  const glm::vec2 head{ WorldToScreen(dir_axis * gizmo->ScreenFactor,
                                      gizmo->ModelViewProjMatrix) };

  g.DrawList->AddLine(tail, head, color, kLineThickness);
#if 1
  constexpr float arrowhead_size{ kLineThickness * 2.0f };
  const glm::vec2 dir{ glm::normalize(gizmo->Origin - head) * arrowhead_size };
  const glm::vec2 orthogonal_dir{ dir.y, -dir.x };
  const glm::vec2 a{ head + dir };
  g.DrawList->AddTriangleFilled(head - dir, a + orthogonal_dir,
                                a - orthogonal_dir, color);
#elif 0
  g.DrawList->AddCircleFilled(head, kCircleRadius, color);
#else
  const glm::vec2 headScaled{ WorldToScreen(
    (dir_axis * gizmo->Scale[axis_idx]) * gizmo->ScreenFactor,
    gizmo->ModelViewProjMatrix) };

  constexpr float kQuadSize{ kLineThickness * 2.0f };
  const glm::vec2 dir{ glm::normalize(gizmo->Origin - headScaled) * kQuadSize };
  const glm::vec2 a{ headScaled + dir }, b{ headScaled - dir };
  const ImVec2 points[4]{ a + glm::vec2{ dir.y, -dir.x },
                          a - glm::vec2{ dir.y, -dir.x },
                          b + glm::vec2{ -dir.y, dir.x },
                          b - glm::vec2{ -dir.y, dir.x } };
  g.DrawList->AddConvexPolyFilled(points, 4, color);
#endif
}
void RenderPlane(ImGuizmoPlane plane_idx, ImGuizmoAxisFlags hover_flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_PlaneYZ + plane_idx) };
  if (gizmo->LockedAxesFlags & PlaneToFlags(plane_idx)) {
    if (g.ConfigFlags & ImGuizmoConfigFlags_HideLocked) return;
    color = GetColorU32(ImGuizmoCol_Inactive);
  } else if (HasPlane(hover_flags) && GetPlaneIdx(hover_flags) == plane_idx) {
    color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);
  }

  ImVec2 plane_points[4];
  for (int i = 0; i < 4; ++i) {
    const glm::vec3 corner_world_space{
      (kUnitDirections[(plane_idx + 1) % 3] * kUnitQuad[i * 2] +
       kUnitDirections[(plane_idx + 2) % 3] * kUnitQuad[i * 2 + 1]) *
      gizmo->ScreenFactor
    };
    plane_points[i] =
      WorldToScreen(corner_world_space, gizmo->ModelViewProjMatrix);
  }

  g.DrawList->AddConvexPolyFilled(plane_points, 4, color);
  constexpr float plane_border{ 1.5f };
  g.DrawList->AddPolyline(plane_points, 4, color | 0x60000000, true,
                          plane_border);
}
void RenderCore(ImGuizmoAxisFlags hover_flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_SpecialMove) };
  if (gizmo->LockedAxesFlags == ImGuizmoAxisFlags_ALL) {
    if (g.ConfigFlags & ImGuizmoConfigFlags_HideLocked) return;
    color = GetColorU32(ImGuizmoCol_Inactive); 
  } else if (hover_flags == ImGuizmoAxisFlags_ALL)
    color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);

  g.DrawList->AddCircleFilled(gizmo->Origin, kCircleRadius, color,
                              kCircleSegmentCount);
}
void RenderTranslationTrail() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const glm::vec2 tail{ WorldToScreen(gizmo->DragTranslationOrigin,
                                      g.Camera.ViewProjectionMatrix) };
  const glm::vec2 head{ WorldToScreen(gizmo->ModelMatrix[3],
                                      g.Camera.ViewProjectionMatrix) };
  const glm::vec2 diff{ glm::normalize(head - tail) * (kCircleRadius - 1.0f) };

  constexpr auto trail_line_color = 0xAAAAAAAA;
  constexpr float margin{ 1.5f };
  g.DrawList->AddCircle(tail, kCircleRadius + margin, trail_line_color);
  g.DrawList->AddCircle(head, kCircleRadius + margin, trail_line_color);
  g.DrawList->AddLine(tail + diff, head - diff, trail_line_color, 2.0f);
}

//-----------------------------------------------------------------------------
// [SECTION] ROTATION
//-----------------------------------------------------------------------------

ImGuizmoAxisFlags FindRotationHover() {
  ImGuizmoAxisFlags hover_flags{ ImGuizmoAxisFlags_None };
  if (IsRotationRingHovered()) hover_flags |= ImGuizmoAxisFlags_ALL;
  if (hover_flags != ImGuizmoAxisFlags_ALL) {
    for (int axis = 0; axis < 3; ++axis) {
      if (IsRotationAxisHovered(axis)) {
        hover_flags |= AxisToFlag(axis, false);
        break;
      }
    }
  }
  // InspectHoverFlags("FindRotationHover()", hover_flags);
  return hover_flags;
}

glm::vec4 BuildRotationPlane() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };

  glm::vec3 point, plane_normal;
  if (HasSingleAxis(flags)) {
    point = gizmo->Mode == ImGuizmoMode_Local ? gizmo->ModelMatrix[3]
                                              : gizmo->SourceModelMatrix[3];
    plane_normal = gizmo->ModelMatrix[GetAxisIdx(flags, false)];
  } else {
    point = gizmo->SourceModelMatrix[3];
    plane_normal = -g.Camera.Forward;
  }
  return BuildPlane(point, plane_normal);
}
void BeginRotation() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  g.DragOrigin = io.MousePos;
  g.BackupModelMatrix = gizmo->SourceModelMatrix;

  gizmo->TranslationPlane = BuildRotationPlane();
  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->RotationVectorSource = glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - gizmo->ModelMatrix[3].xyz);
  gizmo->RotationAngleOrigin = gizmo->ComputeAngleOnPlane();
}
void ContinueRotation(const float *snap) {
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  gizmo->RotationAngle = gizmo->ComputeAngleOnPlane();
  if (snap) ComputeSnap(gizmo->RotationAngle, glm::radians(snap[0]));

  const glm::vec3 rotation_axis_local_space{ glm::normalize(
    glm::mat3{ gizmo->InversedModelMatrix } *
    glm::vec3{ gizmo->TranslationPlane }) };

  float angle = gizmo->RotationAngle - gizmo->RotationAngleOrigin;
  glm::mat4 delta_rotation{ glm::rotate(angle, rotation_axis_local_space) };

  if (gizmo->RotationAngle != gizmo->RotationAngleOrigin) {
    // @todo Handle locked axes ...

    if (gizmo->Mode == ImGuizmoMode_Local) {
      const glm::mat4 scale_origin{ glm::scale(gizmo->ModelScaleOrigin) };
      gizmo->ModelMatrix *= delta_rotation * scale_origin;
    } else {
      glm::mat4 result{ gizmo->SourceModelMatrix };
      result[3] = glm::vec4{ glm::vec3{ 0.0f }, 1.0f };
      gizmo->ModelMatrix = delta_rotation * result;
      gizmo->ModelMatrix[3] = gizmo->SourceModelMatrix[3];
    }
    gizmo->Dirty = true;
  }
  gizmo->RotationAngleOrigin = gizmo->RotationAngle;
}

void RenderRotationAxis(ImGuizmoAxis axis_idx, bool circle,
                        ImGuizmoAxisFlags hover_flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_AxisX + GetAxisAroundIdx(axis_idx)) };
  if (gizmo->LockedAxesFlags & AxisToFlag(axis_idx, true)) {
    if (g.ConfigFlags & ImGuizmoConfigFlags_HideLocked) return;
    color = GetColorU32(ImGuizmoCol_Inactive);
  } else if (HasSingleAxis(hover_flags) &&
             GetAxisIdx(hover_flags, true) == axis_idx) {
    color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);
  }

  glm::vec3 camera_to_model_normalized =
    g.Camera.IsOrtho
      ? -glm::inverse(g.Camera.ViewMatrix)[2]
      : glm::normalize(gizmo->ModelMatrix[3].xyz() - g.Camera.Eye);
  camera_to_model_normalized =
    gizmo->InversedModelMatrix * glm::vec4{ camera_to_model_normalized, 0.0f };

  const float angle_start{ (glm::atan(
                             camera_to_model_normalized[(4 - axis_idx) % 3],
                             camera_to_model_normalized[(3 - axis_idx) % 3])) +
                           kPi * 0.5f };

  glm::vec2 circle_pos[kCircleSegmentCount];
  for (int i = 0; i < kCircleSegmentCount; ++i) {
    const float ng{ angle_start +
                    (circle ? 2 : 1) * kPi *
                      (static_cast<float>(i) / kCircleSegmentCount) };
    const glm::vec3 axis_pos{ glm::cos(ng), glm::sin(ng), 0.0f };
    const auto pos =
      glm::vec3{ axis_pos[axis_idx], axis_pos[(axis_idx + 1) % 3],
                 axis_pos[(axis_idx + 2) % 3] } *
      gizmo->ScreenFactor;
    circle_pos[i] = WorldToScreen(pos, gizmo->ModelViewProjMatrix);
  }
  g.DrawList->AddPolyline(reinterpret_cast<ImVec2 *>(circle_pos),
                          kCircleSegmentCount, color, circle, 2.5f);
}
void RenderRotationRing(ImGuizmoAxisFlags hover_flags) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImU32 color{ GetColorU32(ImGuizmoCol_SpecialMove) };
  if (gizmo->LockedAxesFlags == ImGuizmoAxisFlags_ALL) {
    if (g.ConfigFlags & ImGuizmoConfigFlags_HideLocked) return;
    color = GetColorU32(ImGuizmoCol_Inactive);
  } else if (hover_flags == ImGuizmoAxisFlags_ALL) {
    color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);
  }

  g.DrawList->AddCircle(gizmo->Origin, gizmo->RingRadius, color,
                        kCircleSegmentCount, g.Style.RotationRingThickness);
}

void RenderRotationTrail() {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };

  ImU32 border_color, color;
#if 1
  border_color = GetColorU32(ImGuizmoCol_Hovered);
  color = GetColorU32(ImGuizmoCol_Hovered, 0.541f);
#else
  if (HasSingleAxis(flags)) {
    int axis_idx{ GetAxisIdx(flags, false) };
    color = GetColorU32(ImGuizmoCol_AxisX + axis_idx, 0.541f);
    border_color = GetColorU32(ImGuizmoCol_AxisX + axis_idx);
  } else {
    color = GetColorU32(ImGuizmoCol_SpecialMove, 0.541f);
    border_color = GetColorU32(ImGuizmoCol_SpecialMove);
  }
#endif

  ImVec2 circle_points[kCircleSegmentCount + 1]{ gizmo->Origin };
  for (int i = 1; i < kCircleSegmentCount; ++i) {
    const float ng{ gizmo->RotationAngle *
                    (static_cast<float>(i - 1) / (kCircleSegmentCount - 1)) };
    const glm::mat3 rotate_vector_matrix{ glm::rotate(
      ng, glm::vec3{ gizmo->TranslationPlane }) };
    glm::vec3 pos{ rotate_vector_matrix * gizmo->RotationVectorSource };
    pos *= gizmo->ScreenFactor;
    circle_points[i] = WorldToScreen(pos + gizmo->ModelMatrix[3].xyz(),
                                     g.Camera.ViewProjectionMatrix);
  }

  g.DrawList->AddConvexPolyFilled(circle_points, kCircleSegmentCount, color);
  g.DrawList->AddPolyline(circle_points, kCircleSegmentCount, border_color,
                          true, 2.0f);
}

//-----------------------------------------------------------------------------
// [SECTION] SCALE
//-----------------------------------------------------------------------------

ImGuizmoAxisFlags FindScaleHover() {
  ImGuizmoAxisFlags hover_flags{ ImGuizmoAxisFlags_None };
  if (IsCoreHovered()) hover_flags |= ImGuizmoAxisFlags_ALL;
  if (hover_flags != ImGuizmoAxisFlags_ALL) {
    for (int axis = 0; axis < 3; ++axis)
      if (IsAxisHovered(axis)) {
        hover_flags |= AxisToFlag(axis, false);
        break;
      }
  }
  return hover_flags;
}

glm::vec4 BuildScalePlane() {
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ GetAxisIdx(flags, false) };
    return BuildPlane(gizmo->ModelMatrix[3],
                      gizmo->ModelMatrix[GetScaleAxisIdx(axis_idx)]);
  } else {
    return BuildPlane(gizmo->ModelMatrix[3], gizmo->ModelMatrix[2]);
  }
}

void BeginScale() {
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  g.DragOrigin = ImGui::GetIO().MousePos;
  g.BackupModelMatrix = gizmo->SourceModelMatrix;

  gizmo->Scale = glm::vec3{ 1.0f };
  gizmo->DragTranslationOrigin = gizmo->ModelMatrix[3];
  gizmo->TranslationPlane = BuildScalePlane();
  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  gizmo->TranslationPlaneOrigin = g.Ray.Start + g.Ray.Direction * length;
  gizmo->ModelRelativeOrigin =
    (gizmo->TranslationPlaneOrigin - gizmo->ModelMatrix[3].xyz()) *
    (1.0f / gizmo->ScreenFactor);

  for (int axis = 0; axis < 3; ++axis)
    gizmo->ScaleValueOrigin[axis] = glm::length(gizmo->SourceModelMatrix[axis]);
}
void ContinueScale(const float *snap) {
  const ImGuiIO &io{ ImGui::GetIO() };
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  const float length{ IntersectRayPlane(g.Ray, gizmo->TranslationPlane) };
  const glm::vec3 target_position{ g.Ray.Start + g.Ray.Direction * length };
  const glm::vec3 new_position{ target_position - gizmo->ModelRelativeOrigin *
                                                    gizmo->ScreenFactor };
  glm::vec3 delta{ new_position - gizmo->ModelMatrix[3].xyz };

  const ImGuizmoAxisFlags flags{ gizmo->ActiveManipulationFlags };
  if (HasSingleAxis(flags)) {
    const ImGuizmoAxis axis_idx{ GetAxisIdx(flags, false) };
    const glm::vec3 axis_value{ gizmo->ModelMatrix[axis_idx] };
    const float length_on_axis{ glm::dot(axis_value, delta) };
    delta = axis_value * length_on_axis;
    const glm::vec3 base_vec{ gizmo->TranslationPlaneOrigin -
                              gizmo->ModelMatrix[3].xyz };
    const float ratio{ glm::dot(axis_value, base_vec + delta) /
                       glm::dot(axis_value, base_vec) };
    // @todo Support for scale in Global mode
    gizmo->Scale[axis_idx] = glm::max(ratio, 0.001f);
  } else {
    const float scale_delta{ (io.MousePos.x - g.DragOrigin.x) * 0.01f };
    gizmo->Scale = glm::vec3{ glm::max(1.0f + scale_delta, 0.001f) };
  }

  if (snap) {
    const float scale_snap[]{ snap[0], snap[0], snap[0] };
    ComputeSnap(gizmo->Scale, scale_snap);
  }

  for (int axis = 0; axis < 3; ++axis) {
    gizmo->Scale[axis] = glm::max(gizmo->Scale[axis], 0.001f);
    if (gizmo->LockedAxesFlags & AxisToFlag(axis, false))
      gizmo->Scale[axis] = 1.0f;
  }

  if (gizmo->LastScale != gizmo->Scale) {
    gizmo->ModelMatrix *= glm::scale(gizmo->Scale * gizmo->ScaleValueOrigin);
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
  ImGuizmoWidget *gizmo{ g.CurrentGizmo };

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
    ImGui::Text("ActiveManipulation: %d", gizmo->ActiveManipulationFlags);

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
      ImGui::InputFloat3("LastTranslationDelta",
                         &gizmo->LastTranslationDelta[0]);
      ImGui::InputFloat3("ModelRelativeOrigin", &gizmo->ModelRelativeOrigin[0]);
      ImGui::InputFloat3("TranslationPlaneOrigin",
                         &gizmo->TranslationPlaneOrigin[0]);

      ImGui::PopItemFlag();
      ImGui::TreePop();
    }
    if (ImGui::TreeNode("Rotation")) {
      ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);

      ImGui::InputFloat3("RotationVectorSource",
                         &gizmo->RotationVectorSource[0]);
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

bool Manipulate(ImGuizmoMode mode, ImGuizmoOperation operation, float *model,
                const float *snap) {
  if (Begin(mode, model)) {
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
  }
  PrintContext();
  return End();
}

void SetCamera(const float *view, const float *projection, bool isOrtho) {
  IM_ASSERT(view && projection);

  ImGuizmoContext &g{ GImGuizmo };

  g.Camera.ViewMatrix = glm::make_mat4(view);
  const glm::mat4 inversed_view_matrix{ glm::inverse(g.Camera.ViewMatrix) };
  g.Camera.Right = inversed_view_matrix[0];
  g.Camera.Up = inversed_view_matrix[1];
  g.Camera.Forward = inversed_view_matrix[2];
  g.Camera.Eye = inversed_view_matrix[3];

  g.Camera.IsOrtho = isOrtho;
  g.Camera.ProjectionMatrix = glm::make_mat4(projection);

  g.Camera.ViewProjectionMatrix =
    g.Camera.ProjectionMatrix * g.Camera.ViewMatrix;
}

bool Begin(ImGuizmoMode mode, float *model, ImGuizmoAxisFlags locked_axes) {
  IM_ASSERT(model && "Model matrix required");

  ImGuizmoContext &g{ GImGuizmo };
  IM_ASSERT(!g.LockedModelMatrix && "Nesting forbidden");
  g.LockedModelMatrix = model;
  //IM_ASSERT(g.DrawList);
  if (g.DrawList == nullptr) g.DrawList = ImGui::GetWindowDrawList();
  
  const glm::vec2 win_pos{ ImGui::GetWindowPos() };
  const glm::vec2 region_min{ ImGui::GetWindowContentRegionMin() };
  const glm::vec2 region_max{ ImGui::GetWindowContentRegionMax() };
  SetViewport(win_pos + region_min, region_max - region_min);

  auto id = static_cast<ImGuiID>(reinterpret_cast<long long>(model));
  ImGuizmoWidget *gizmo{ FindGizmoById(id) };
  if (gizmo == nullptr) gizmo = CreateNewGizmo(id);
  g.CurrentGizmo = gizmo;

  if (!ImGui::GetIO().MouseDown[0]) {
    g.ActiveGizmo = nullptr;
    gizmo->ActiveManipulationFlags = ImGuizmoAxisFlags_None;
  }
  if (!gizmo->ActiveManipulationFlags)
    gizmo->ActiveOperation = ImGuizmoOperation_None;

  gizmo->Mode = mode;
  gizmo->LockedAxesFlags = locked_axes;
  gizmo->Load(model);
  g.Ray = RayCast(g.Camera.ViewProjectionMatrix);

  const glm::vec3 camera_space_position{ gizmo->ModelViewProjMatrix *
                                         glm::vec4{ glm::vec3{ 0.0f }, 1.0f } };
  return g.Camera.IsOrtho ? true : camera_space_position.z >= 0.001f;
}
bool End() {
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };
  IM_ASSERT(g.LockedModelMatrix && "It seems that you didn't call Begin()");

  if (io.MouseDown[1] && gizmo->ActiveManipulationFlags) {
    gizmo->ModelMatrix = g.BackupModelMatrix;
    gizmo->Dirty = true;
    gizmo->ActiveManipulationFlags = ImGuizmoAxisFlags_None;
  }

  bool updated{ false };
  if (gizmo->Dirty) {
    *reinterpret_cast<glm::mat4 *>(g.LockedModelMatrix) = gizmo->ModelMatrix;
    gizmo->Dirty = false;
    updated = true;
  }
  g.LockedModelMatrix = nullptr;
  g.CurrentGizmo = nullptr;

  return updated;
}

void Translate(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoAxisFlags hover_flags{ FindTranslationHover() };
  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Translate, hover_flags, &held))
    BeginTranslation();
  if (held) ContinueTranslation(snap);

  if (gizmo->ActiveOperation == ImGuizmoOperation_Translate)
    RenderTranslationTrail();

  if (!gizmo->ActiveManipulationFlags ||
      !(g.ConfigFlags & ImGuizmoConfigFlags_CloakOnManipulate)) {
    for (int axis = 0; axis < 3; ++axis)
      RenderAxis(axis, hover_flags);
    for (int plane = 0; plane < 3; ++plane)
      RenderPlane(plane, hover_flags);
    RenderCore(hover_flags);
  }

  if (gizmo->ActiveOperation == ImGuizmoOperation_Translate)
    RenderTranslationInfo();
}
void Rotate(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  const ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoAxisFlags hover_flags{ FindRotationHover() };

  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Rotate, hover_flags, &held))
    BeginRotation();
  if (held) ContinueRotation(snap);

  if (gizmo->ActiveManipulationFlags &&
      (g.ConfigFlags & ImGuizmoConfigFlags_CloakOnManipulate)) {
    if (HasSingleAxis(hover_flags))
      RenderRotationAxis(GetAxisIdx(hover_flags, true), true, hover_flags);
    else if (hover_flags == ImGuizmoAxisFlags_ALL) {
      RenderRotationRing(hover_flags);
    }
  } else {
    for (int axis = 0; axis < 3; ++axis)
      RenderRotationAxis(axis, false, hover_flags);
    RenderRotationRing(hover_flags);
  }

  if (gizmo->ActiveOperation == ImGuizmoOperation_Rotate) {
    RenderRotationTrail();
    RenderRotationInfo();
  }
}
void Scale(const float *snap) {
  const ImGuizmoContext &g{ GImGuizmo };
  ImGuizmoWidget *gizmo{ GetCurrentGizmo() };

  ImGuizmoAxisFlags hover_flags{ FindScaleHover() };
  bool held;
  if (GizmoBehavior(ImGuizmoOperation_Scale, hover_flags, &held)) {
    gizmo->Mode = ImGuizmoMode_Local;
    gizmo->Load(g.LockedModelMatrix);
    BeginScale();
  }
  if (held) {
    gizmo->Mode = ImGuizmoMode_Local;
    gizmo->Load(g.LockedModelMatrix);
    ContinueScale(snap);
  }

  if (!gizmo->ActiveManipulationFlags ||
      !(g.ConfigFlags & ImGuizmoConfigFlags_CloakOnManipulate)) {
    for (int axis = 0; axis < 3; ++axis)
      RenderAxis(axis, hover_flags);
    RenderCore(hover_flags);
  }

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

  // ImGui::GetItemID();
  ImGuiID parentId{ ImGui::GetCurrentWindow()->ID };
  // ImGui::GetCurrentContext()->ActiveIdWindow

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
  for (int axis = 0; axis < 3; ++axis) {
    const auto valid_scale =
      glm::abs(scale[axis]) < kEpsilon ? 0.001f : scale[axis];
    mat[axis] *= valid_scale;
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

void ImGuizmoWidget::Load(const float *model) {
  const ImGuizmoContext &g{ GImGuizmo };

  SourceModelMatrix = glm::make_mat4(model);
  if (Mode == ImGuizmoMode_Local) {
    ModelMatrix = SourceModelMatrix;
    for (int axis = 0; axis < 3; ++axis)
      ModelMatrix[axis] = glm::normalize(ModelMatrix[axis]);
  } else {
    ModelMatrix = glm::translate(SourceModelMatrix[3].xyz());
  }
  ModelViewProjMatrix = g.Camera.ViewProjectionMatrix * ModelMatrix;

  for (int axis = 0; axis < 3; ++axis)
    ModelScaleOrigin[axis] = glm::length(SourceModelMatrix[axis]);

  InversedModelMatrix = glm::inverse(ModelMatrix);


  glm::vec4 pointRight = InversedModelMatrix[0];
  pointRight.w = 1.0f;
  pointRight = g.Camera.ViewProjectionMatrix * pointRight;

  auto sf =
    g.Style.GizmoScale / (pointRight.x / pointRight.w -
                          ModelViewProjMatrix[3].x / ModelViewProjMatrix[3].w);

  const glm::vec3 right_view_inverse{ InversedModelMatrix *
                                      glm::vec4{ g.Camera.Right, 0.0f } };
  const float right_length{ GetSegmentLengthClipSpace(glm::vec3{ 0.0f },
                                                      right_view_inverse) };
  ScreenFactor = g.Style.GizmoScale / right_length;
  Origin = WorldToScreen(glm::vec3{ 0.0f }, ModelViewProjMatrix);
  RingRadius = g.Style.GizmoScale /* 1.04f*/ * g.Viewport.GetWidth() * 0.55f;
  //RingRadius = 0.06 * g.Viewport.GetHeight();
}

float ImGuizmoWidget::ComputeAngleOnPlane() const {
  const ImGuizmoContext &g{ GImGuizmo };

  const float length{ IntersectRayPlane(g.Ray, TranslationPlane) };
  const glm::vec3 local_pos{ glm::normalize(
    g.Ray.Start + g.Ray.Direction * length - ModelMatrix[3].xyz) };

  const glm::vec3 perpendicular_vec{ glm::normalize(
    glm::cross(RotationVectorSource, TranslationPlane.xyz())) };

  const float acos_angle{ glm::clamp(glm::dot(local_pos, RotationVectorSource),
                                     -1.0f, 1.0f) };
  float angle{ glm::acos(acos_angle) };
  angle *= (glm::dot(local_pos, perpendicular_vec) < 0.0f) ? 1.0f : -1.0f;
  return angle;
}