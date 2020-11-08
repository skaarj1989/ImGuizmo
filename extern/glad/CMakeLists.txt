add_library(glad STATIC)
target_sources(glad
  PRIVATE
		"src/glad.c"
		"src/glad_wgl.c"
)
target_include_directories(glad PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")