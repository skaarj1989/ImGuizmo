cmake_minimum_required(VERSION 3.13.4...3.19)
project(ImGuizmo)
if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there."
  )
endif()

#list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_SOURCE_DIR}/cmake")
set_property(GLOBAL PROPERTY USE_FOLDERS YES)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_DEBUG_POSTFIX "_d")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib") # .lib
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib") # .dll
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin") # .exe

if(WIN32)
  message(STATUS "Generate build for WIN32")
  add_definitions(
    "-DVC_EXTRALEAN"
    "-DWIN32_LEAN_AND_MEAN"
  )
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  message(STATUS "Generate build for Linux")
  set(LINUX YES)
endif()

set(glm_DIR "${CMAKE_SOURCE_DIR}/extern/glm/cmake/glm")
find_package(glm REQUIRED)
add_subdirectory(extern)

add_library(ImGuizmo )
target_sources(ImGuizmo PRIVATE "ImGuizmo.h" "ImGuizmo.cpp")
target_include_directories(ImGuizmo PUBLIC ${CMAKE_SOURCE_DIR})
target_link_libraries(ImGuizmo
	PUBLIC
		imgui
	PRIVATE
		glm::glm
)

add_subdirectory(examples)

include(GNUInstallDirs)