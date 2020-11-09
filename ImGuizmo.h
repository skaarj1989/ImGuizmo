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
  float ScreenRingSize{ 0.06f };

  float Alpha{ 1.0f };
  ImVec4 Colors[ImGuizmoCol_COUNT];

  IMGUI_API ImGuizmoStyle();
};

enum ImGuizmoMode_ {
  ImGuizmoMode_Local,
  ImGuizmoMode_World,

  ImGuizmoMode_COUNT
};
enum ImGuizmoOperation_ {
  ImGuizmoOperation_Translate,
  ImGuizmoOperation_Rotate,
  ImGuizmoOperation_Scale,

  ImGuizmoOperation_Bounds,

  ImGuizmoOperation_COUNT
};

namespace ImGuizmo {

IMGUI_API ImGuizmoStyle &GetStyle(); 

// call inside your own window and before Manipulate() in order to draw gizmo to
// that window. Or pass a specific ImDrawList to draw to (e.g.
// ImGui::GetForegroundDrawList()).
IMGUI_API void SetDrawlist(ImDrawList *drawlist = nullptr);

/** call BeginFrame right after ImGui_XXXX_NewFrame(); */
IMGUI_API void BeginFrame();

/** @return if mouse is over any gizmo control (axis, plane or screen component) */
IMGUI_API bool IsOver();

/** @return true if mouse IsOver or if the gizmo is in moving state */
IMGUI_API bool IsUsing();

// enable/disable the gizmo. Stay in the state until next call to Enable.
// gizmo is rendered with gray half transparent color when disabled
IMGUI_API void Enable(bool enabled);

// helper functions for manualy editing translation/rotation/scale with an input
// float translation, rotation and scale float points to 3 floats each Angles
// are in degrees (more suitable for human editing) example: float
// matrixTranslation[3], matrixRotation[3], matrixScale[3];
// ImGuizmo::DecomposeMatrixToComponents(gizmoMatrix.m16, matrixTranslation,
// matrixRotation, matrixScale); ImGui::InputFloat3("Tr", matrixTranslation, 3);
// ImGui::InputFloat3("Rt", matrixRotation, 3);
// ImGui::InputFloat3("Sc", matrixScale, 3);
// ImGuizmo::RecomposeMatrixFromComponents(matrixTranslation, matrixRotation,
// matrixScale, gizmoMatrix.m16);
//
// These functions have some numerical stability issues for now. Use with
// caution.

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

IMGUI_API void SetViewport(const ImVec2 &position, const ImVec2 &size);
IMGUI_API void SetViewport(float x, float y, float width, float height);

IMGUI_API void SetOrthographic(bool isOrthographic);

// Render a cube with face color corresponding to face normal. Usefull for
// debug/tests
IMGUI_API void DrawCubes(const float *view, const float *projection,
                         const float *matrices, int matrixCount);
IMGUI_API void DrawGrid(const float *view, const float *projection,
                        const float *matrix, const float gridSize);

// call it when you want a gizmo
// Needs view and projection matrices.
// matrix parameter is the source matrix (where will be gizmo be drawn) and
// might be transformed by the function. Return deltaMatrix is optional
// translation is applied in world space

/**
 * @param [in] view Camera view, column-major matrix
 * @param [in] projection Camera projection, column-major matrix
 * @param [in/out] matrix Model, column-major matrix
 * @param [in] snap
 */
IMGUI_API bool Manipulate(const float *view, const float *projection,
                          ImGuizmoOperation_ operation, ImGuizmoMode_ mode,
                          float *model, float *deltaMatrix = nullptr,
                          const float *snap = nullptr,
                          const float *localBounds = nullptr,
                          const float *boundsSnap = nullptr);
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

IMGUI_API void SetID(int id);

/** @return true if the cursor is over the operations gizmo */
IMGUI_API bool IsOver(ImGuizmoOperation_ op);

}; // namespace ImGuizmo
