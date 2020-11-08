add_executable(ImGuizmoSample "main.cpp")
target_link_libraries(ImGuizmoSample
	PRIVATE
		glm::glm
		ImGuizmo
)