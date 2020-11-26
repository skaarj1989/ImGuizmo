### ImGuizmo

[![CodeFactor Grade](https://img.shields.io/codefactor/grade/github/skaarj1989/ImGuizmo)](https://www.codefactor.io/repository/github/skaarj1989/imguizmo/overview/master)
![GitHub](https://img.shields.io/github/license/skaarj1989/ImGuizmo.svg)


![sample code output (classic)](https://github.com/skaarj1989/ImGuizmo/blob/gh-pages/example.gif?raw=true)

#### Gizmo tools

Gizmo must be created inside of target window (e.g *Scene Window*)

Minimal example (translate operation, gizmo aligned to world grid)
```cpp
// Separate window for scene
if (ImGui::Begin("Scene")) {
  ImGui::Image(scene_texture, ImGui::GetContentRegionAvail());
  // ImGuizmo will capture DrawList of this window
  ImGuizmo::Manipulate(ImGuizmoMode_World, ImGuizmoOperation_Translate, model_matrix);
}
ImGui::End();
```

```cpp
// Invisible window over main framebuffer
static const ImGuiViewport *main_viewport{ ImGui::GetMainViewport() };
ImGui::SetNextWindowPos(main_viewport->Pos);
ImGui::SetNextWindowSize(main_viewport->Size);
    
ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
ImGui::PushStyleColor(ImGuiCol_Border, 0);
ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

constexpr ImGuiWindowFlags window_flags{
  ImGuiWindowFlags_NoBackground | ImGuiWindowFlags_NoDecoration |
  ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoDocking |
  ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoBringToFrontOnFocus
};
if (ImGui::Begin("Canvas", nullptr, window_flags)) {
  ImGuizmo::Begin(ImGuizmoMode_World, model_matrix);
  ImGuizmo::Tranlsate();
  ImGuizmo::End();
}
ImGui::End();
ImGui::PopStyleVar();
ImGui::PopStyleColor(2);
```

#### View manipulator

```cpp
// Use whole window
ImGui::SetNextWindowSize(glm::vec2{ 256 });
if (ImGui::Begin("View", nullptr, ImGuiWindowFlags_NoResize)) {
  ImGuizmo::ViewManipulate(view_matrix, cam_distance);
}
ImGui::End();
```

```cpp
// Use part of window (top-right)
constexpr ImVec2 manip_size{ 128, 128 };
ImVec2 position{ main_viewport->Pos };
position.x += main_viewport->Size.x - manip_size.x;
ImGuizmo::ViewManipulate(view_matrix, cam_distance, position, manip_size);
```

#### Build examples
```bash
mkdir build && cd build
cmake ..
```

#### Installation

Copy `ImGuizmo.h` and `ImGuizmo.cpp` into folder with **imgui**

#### Customization

You can modify colors and scale.

_Predefined themes:_`StyleColorsClassic()` `StyleColorsBlender()` `StyleColorsUnreal()`
<br>![sample code output (classic)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/classic.png) ![sample code output (blender)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/blender.png) ![sample code output (unreal)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/unreal.png)

Change behavior via `SetConfigFlags()`
* Reverse operation on right mouse button click
* Hide gizmo on use
* Lock axes (only for translate and rotate for now)

Build your gizmo from blocks:
```cpp
Translate();
Rotate();
Scale();
BoundsScale();
```

#### Refactoring
- Replaced custom math with **glm**
- Reduced cognitive complexity

### License

ImGuizmo is licensed under the MIT License, see LICENSE for more information.
