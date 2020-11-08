add_executable(ImGuizmoSample)
target_sources(ImGuizmoSample
  PRIVATE
    "imgui_impl_glfw.h"
    "imgui_impl_glfw.cpp"
		"imgui_impl_opengl3.h"
		"imgui_impl_opengl3.cpp"
		"main.cpp"
)
target_compile_definitions(ImGuizmoSample PUBLIC IMGUI_IMPL_OPENGL_LOADER_GLAD)
target_link_libraries(ImGuizmoSample
	PRIVATE
	  glfw
		glad
		glm::glm
		ImGuizmo
)