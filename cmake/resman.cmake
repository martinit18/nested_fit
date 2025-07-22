# Brief  : Create resources to be attached to nested_fit at compile time
# Author : César Godinho
# Date   : 25/07/25

add_executable(rmhex
	src/helper/rmhex.cpp
)

function(rm_link_resources)
	cmake_parse_arguments(
		PARG
		""
		"TARGET;OUTDIR"
		"FILES;DEPENDS"
		${ARGN}
	)

	if(NOT PARG_TARGET)
		message(FATAL_ERROR "rm_link_resources: A target is required.")
	endif()

	if(NOT PARG_FILES)
		message(FATAL_ERROR "rm_link_resources: At least one resource file is required.")
	endif()

	set(LIB_SRC_FILES "")
	set(RES_DEFS "")
	set(RES_NAMES "")
	set(RES_LENS "")
	set(RES_PATHS "")

	file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/resman)
	foreach(res ${PARG_FILES})
		get_filename_component(res_dir ${res} DIRECTORY)
		get_filename_component(res_name ${res} NAME)
		get_filename_component(full_path ${res} REALPATH)
		set(output_file ${CMAKE_BINARY_DIR}/resman/${res_name}.cpp)

		add_custom_command(
			OUTPUT ${output_file}
			#COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/cmake/generate_hex_header.cmake
			COMMAND rmhex ${res_name} ${output_file}
			DEPENDS ${full_path}
			COMMENT "Generating resource from: ${res_name}"
			WORKING_DIRECTORY ${res_dir}
		)

		list(APPEND LIB_SRC_FILES ${output_file})

		# Add resource def
		string(REPLACE "." "_" fqn_name ${res_name})
		list(APPEND RES_DEFS "RES_EXT_DEFINE(${fqn_name})")

		# Add resource name
		list(APPEND RES_NAMES "RES_VAR(${fqn_name})")

		# Add resource length
		list(APPEND RES_LENS "RES_LEN(${fqn_name})")

		# Add resource path
		if(PARG_OUTDIR)
			list(APPEND RES_PATHS "\"${PARG_OUTDIR}/${res_name}\"")
		else()
			list(APPEND RES_PATHS "\"${res_name}\"")
		endif()
	endforeach()

	string(JOIN ";\n" RES_DEFS ${RES_DEFS})
	string(JOIN ",\n\t\t" RES_NAMES ${RES_NAMES})
	string(JOIN ",\n\t\t" RES_LENS ${RES_LENS})
	string(JOIN ",\n\t\t" RES_PATHS ${RES_PATHS})
	configure_file(src/helper/resman.cpp.in resman.cpp @ONLY)

	add_library(_resman_lib STATIC ${LIB_SRC_FILES} ${CMAKE_BINARY_DIR}/resman.cpp)
	if(PARG_DEPENDS)
		add_dependencies(_resman_lib ${PARG_DEPENDS})
		add_dependencies(_resman_lib rmhex)
	endif()

	target_link_libraries(${PARG_TARGET} PRIVATE _resman_lib)
endfunction()
