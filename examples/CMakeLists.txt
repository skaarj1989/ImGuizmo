add_subdirectory(simple)
add_subdirectory(editor)

set_target_properties(Simple Editor
	PROPERTIES
		FOLDER "examples"
)