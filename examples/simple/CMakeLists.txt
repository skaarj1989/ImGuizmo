add_executable(Simple)
target_sources(Simple
  PRIVATE
    "../backends/imgui_impl_glfw.h"
    "../backends/imgui_impl_glfw.cpp"
		"../backends/imgui_impl_opengl3.h"
		"../backends/imgui_impl_opengl3.cpp"
		"main.cpp"
)
target_compile_definitions(Simple PUBLIC IMGUI_IMPL_OPENGL_LOADER_GLAD)
target_link_libraries(Simple
	PRIVATE
	  glfw
		glad
		glm::glm
		ImGuizmo
)