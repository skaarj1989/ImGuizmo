add_executable(Editor)
target_sources(Editor
  PRIVATE
    "../backends/imgui_impl_glfw.h"
    "../backends/imgui_impl_glfw.cpp"
		"../backends/imgui_impl_opengl3.h"
		"../backends/imgui_impl_opengl3.cpp"
		"main.cpp"
)
target_compile_definitions(Editor PUBLIC IMGUI_IMPL_OPENGL_LOADER_GLAD)
target_link_libraries(Editor
	PRIVATE
	  glfw
		glad
		glm::glm
		ImGuizmo
)


