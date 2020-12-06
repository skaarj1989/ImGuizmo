set(GLFW_BUILD_EXAMPLES OFF)
set(GLFW_BUILD_TESTS OFF)
set(GLFW_BUILD_DOCS OFF)
set(GLFW_INSTALL OFF)
add_subdirectory(glfw)

add_subdirectory(glad)

set(imgui_DIR "${CMAKE_CURRENT_SOURCE_DIR}/imgui")
add_library(imgui STATIC)
target_sources(imgui
  PRIVATE
    "${imgui_DIR}/imconfig.h"
    "${imgui_DIR}/imgui_internal.h"
    "${imgui_DIR}/imgui.h"
    "${imgui_DIR}/imstb_rectpack.h"
    "${imgui_DIR}/imstb_textedit.h"
    "${imgui_DIR}/imstb_truetype.h"
    "${imgui_DIR}/imgui.cpp"
    "${imgui_DIR}/imgui_draw.cpp"
    "${imgui_DIR}/imgui_widgets.cpp"
    "${imgui_DIR}/imgui_tables.cpp"
    "${imgui_DIR}/imgui_demo.cpp"
)
target_include_directories(imgui PUBLIC ${imgui_DIR})
target_link_libraries(imgui PUBLIC glm::glm)

set_target_properties(glfw glad imgui
	PROPERTIES
		FOLDER "extern"
)