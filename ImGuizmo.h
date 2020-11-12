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

#pragma once

#include "imgui.h"

enum ImGuizmoCol_ {
  ImGuizmoCol_Text,
  ImGuizmoCol_TextShadow,

  ImGuizmoCol_Inactive,
  ImGuizmoCol_Selection,

  ImGuizmoCol_AxisX,
  ImGuizmoCol_AxisY,
  ImGuizmoCol_AxisZ,

  ImGuizmoCol_PlaneYZ,
  ImGuizmoCol_PlaneZX,
  ImGuizmoCol_PlaneXY,

  ImGuizmoCol_COUNT
};
struct ImGuizmoStyle {
  float GizmoScale{ 0.1f };
  float RotationRingThickness{ 4.5f };

  float Alpha{ 1.0f };
  ImVec4 Colors[ImGuizmoCol_COUNT];

  IMGUI_API ImGuizmoStyle();
};

enum ImGuizmoMode_ {
  ImGuizmoMode_Local,
  ImGuizmoMode_Global,

  ImGuizmoMode_COUNT
};

enum ImGuizmoOperationFlags_ {
  ImGuizmoOperationFlags_None = 0,

  ImGuizmoOperationFlags_Translate = 1 << 0,
  ImGuizmoOperationFlags_Rotate = 1 << 1,
  ImGuizmoOperationFlags_Scale = 1 << 2,
};

using ImGuizmoOperationFlags = int;

namespace ImGuizmo {

IMGUI_API void PrintContext();

IMGUI_API ImGuizmoStyle &GetStyle(); 
IMGUI_API void StyleColorsDefault(ImGuizmoStyle *dst);

IMGUI_API void Enable(bool enabled);

/** @param drawList */
IMGUI_API void SetDrawlist(ImDrawList *drawList = nullptr);
IMGUI_API void SetViewport(const ImVec2 &position, const ImVec2 &size);
IMGUI_API void SetViewport(float x, float y, float width, float height);

/**
 * @note Convenience function
 * @brief Creates transparent window and uses its drawList and viewport
 */
IMGUI_API void SetupWorkspace(const char *name, const ImVec2 &position,
                              const ImVec2 &size);

/**
 * @param [in] view Camera view matrix (column-major)
 * @param [in] projection Camera projection matrix (column-major)
 */
IMGUI_API void SetCamera(const float *view, const float *projection,
                         bool isOrtho);

/**
 * @param [in/out] matrix Model matrix (column-major)
 * @param [out] deltaMatrix 
 * @param [in] snap vec3
 */
IMGUI_API bool Manipulate(ImGuizmoMode_ mode, ImGuizmoOperationFlags flags,
                          float *model, float *deltaMatrix = nullptr,
                          const float *snap = nullptr);

/**
 * @note Please note that this cubeview is patented by Autodesk:
 * https://patents.google.com/patent/US7782319B2/en It seems to be a defensive
 * patent in the US. I don't think it will bring troubles using it as other
 * software are using the same mechanics. But just in case, you are now warned!
 * 
 * @param [in] view Camera view, column-major matrix
 */
IMGUI_API void ViewManipulate(float *view, const float length, ImVec2 position,
                              ImVec2 size, ImU32 backgroundColor);

/** @return true if mouse IsOver or if the gizmo is in moving state */
IMGUI_API bool IsUsing();
/** @return if mouse is over any gizmo control (axis, plane or screen component)
 */
//IMGUI_API bool IsOver();
/** @return true if the cursor is over the operations gizmo */
//IMGUI_API bool IsOver(ImGuizmoOperation_ op);

//
//
//

IMGUI_API void DrawCubes(const float *view, const float *projection,
                         const float *models, int modelCount);
IMGUI_API void DrawGrid(const float *view, const float *projection,
                        const float *model, const float gridSize);

/**
 * @param [in] matrix Column-major matrix
 * @param [out] t vec3 Translation
 * @param [out] r vec3 Rotation
 * @param [out] s vec3 Scale
 */
IMGUI_API void DecomposeMatrix(const float *matrix, float *t, float *r,
                               float *s);
/**
 * @param [in] t vec3 Translation
 * @param [in] r vec3 Rotation
 * @param [in] s vec3 Scale
 * @param [out] matrix Column-major matrix
 */
IMGUI_API void RecomposeMatrix(const float *t, const float *r, const float *s,
                               float *matrix);

}; // namespace ImGuizmo