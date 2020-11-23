### ImGuizmo

#### Usage

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

#### Build examples
```bash
mkdir build && cd build
cmake ..
```

#### Installation

Copy `ImGuizmo.h` and `ImGuizmo.cpp` into folder with **imgui**

#### Customization

<br>![sample code output (classic)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/classic.png) ![sample code output (blender)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/blender.png) ![sample code output (unreal)](https://raw.githubusercontent.com/skaarj1989/ImGuizmo/gh-pages/images/unreal.png)
_Themes:_`StyleColorsClassic()` `StyleColorsBlender()` `StyleColorsUnreal()`

#### Features

Available via `SetConfigFlags()`
* Reverse operation on right mouse button click
* 
* Lock axes (only for translate and rotate for now)

### License

ImGuizmo is licensed under the MIT License, see LICENSE for more information.
