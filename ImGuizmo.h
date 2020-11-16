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

  ImGuizmoCol_SpecialMove,

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
using ImGuizmoMode = int;

enum ImGuizmoOperation_ {
  ImGuizmoOperation_None,

  ImGuizmoOperation_Translate,
  ImGuizmoOperation_Rotate,
  ImGuizmoOperation_Scale,
};
using ImGuizmoOperation = int;

namespace ImGuizmo {

IMGUI_API void PrintContext();

IMGUI_API ImGuizmoStyle &GetStyle(); 
IMGUI_API void StyleColorsDefault(ImGuizmoStyle *dst);

IMGUI_API void SetViewport(const ImVec2 &position, const ImVec2 &size);
IMGUI_API void SetViewport(float x, float y, float width, float height);
/** @param drawList */
IMGUI_API void SetDrawlist(ImDrawList *drawList = nullptr);

/**
 * @note Convenience method
 * @brief Creates transparent window and uses its DrawList and dimensions
 */
IMGUI_API void CreateCanvas(const char *name);
IMGUI_API void CreateCanvas(const char *name, const ImVec2 &position,
                            const ImVec2 &size);
/** @note Convenience method */
IMGUI_API void Manipulate(ImGuizmoMode mode, ImGuizmoOperation operation,
                          float *model, const float *snap = nullptr);

/**
 * @param [in] view Camera view matrix (column-major)
 * @param [in] projection Camera projection matrix (column-major)
 */
IMGUI_API void SetCamera(const float *view, const float *projection,
                         bool isOrtho);

/** 
 * @param [in] model Model matrix (column-major)
 */
IMGUI_API bool Begin(ImGuizmoMode mode, float *model);
/**
 * Saves result to locked model matrix if manipulation has been made between
 * Begin/End
 */
IMGUI_API void End();

/** @param snap */
IMGUI_API void Translate(const float *snap = nullptr);
/** @param snap */
IMGUI_API void Rotate(const float *snap = nullptr);
/** @param snap */
IMGUI_API void Scale(const float *snap = nullptr);

/** @param snap */
IMGUI_API void Cage(const float *bounds, const float *snap);

IMGUI_API void ViewManip(float *view, const float length,
                         const ImVec2 &position, const ImVec2 &size,
                         ImU32 backgroundColor = 0x10101010);

/**
 * @note Please note that this cubeview is patented by Autodesk:
 * https://patents.google.com/patent/US7782319B2/en It seems to be a defensive
 * patent in the US. I don't think it will bring troubles using it as other
 * software are using the same mechanics. But just in case, you are now warned!
 * 
 * @param [in] view Camera view, column-major matrix
 */
IMGUI_API void ViewManipulate(float *view, const float length,
                              const ImVec2 &position, const ImVec2 &size,
                              ImU32 backgroundColor);

//
//
//

/** @todo Remove me */
IMGUI_API void DrawCubes(const float *view, const float *projection,
                         const float *models, int modelCount);
/** @todo Remove me */
IMGUI_API void DrawGrid(const float *view, const float *projection,
                        const float *model, const float gridSize);

/**
 * @param [in] matrix Input matrix (column-major)
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
 * @param [out] matrix Result matrix (column-major)
 */
IMGUI_API void RecomposeMatrix(const float *t, const float *r, const float *s,
                               float *matrix);

}; // namespace ImGuizmo