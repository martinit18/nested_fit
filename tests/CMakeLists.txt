# NOTE(César): Testing requires python, I could run it off Fortran or even C++, but I don't see the need to do so.
find_package(Python COMPONENTS Interpreter REQUIRED QUIET)

message(STATUS "Configuring tests directory.")

set(nf_test_version_input ${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR})

macro(listsubdir result curdir)
    file(GLOB children RELATIVE ${curdir} ${curdir}/*)
    set(dirlist "")
    foreach(child ${children})
        if(IS_DIRECTORY ${curdir}/${child} AND NOT (child STREQUAL "expect")) # Ignore the expect test
            list(APPEND dirlist ${child})
        endif()
    endforeach()
    set(${result} ${dirlist})
endmacro()

macro(add_test_dir_meta tname ttype)
    # Are we using openMPI to test ?
    if(OPENMPI)
        set(nf_test_mt_type "OpenMPI")
        # If we are using OpenMPI use at least NCORES tries (?)
        cmake_host_system_information(RESULT NCORES QUERY NUMBER_OF_PHYSICAL_CORES)
    else()
        set(nf_test_mt_type "Baseline")
        # Else just set the number of tries to the default
        if(NOT ${ttype} STREQUAL "data")
            set(NCORES 3) # 3 For functions
            list(APPEND nf_test_bin ${nested_fit_target})
        else()
            set(NCORES 1) # 1 For data
        endif()
    endif()

    if(NOT ${ttype} STREQUAL "data")
        list(APPEND nf_test_bin ${nested_fit_target_func})
    else()
        list(APPEND nf_test_bin ${nested_fit_target})
    endif()

    # How many tries should we use ?
    set(nf_test_current_ntries ${NCORES})
    message(STATUS "Configuring test -> ${tname} | ${nf_test_mt_type} | ${NCORES} try/tries")
endmacro()

macro(generate_test_subfolders ttype)
    # This is a data test directory
    listsubdir(data_test_dirs ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}/)
    foreach(data_test ${data_test_dirs})
        add_test_dir_meta(${data_test} ${ttype})
        execute_process(COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/configure.py
            ${nf_test_version_input}
            ${NCORES}
            ${CMAKE_BINARY_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}
            ${data_test}
        )
        message(STATUS "")
    endforeach()
    listsubdir(data_test_build_dirs ${CMAKE_BINARY_DIR}/tests/)
    foreach(data_test_build ${data_test_build_dirs})
        list(APPEND nf_test_folders ${data_test_build})
    endforeach()
endmacro()

set(nf_test_bin "")
set(nf_test_folders "")

# Make the json loading part of the code available to the cmake generated modules
# list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/tests/)
# file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/TryJsonField.cmake DESTINATION ${CMAKE_BINARY_DIR}/tests/)

# Finaly generate all of the test folders
generate_test_subfolders("data")
generate_test_subfolders("func")


# Add unit Testing
add_subdirectory(unit)


# Copy the test suite file
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test_suite.py DESTINATION ${CMAKE_BINARY_DIR}/tests/)

# Configure the python test file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test.py ${CMAKE_BINARY_DIR}/test.py)

add_custom_target(test-all
    COMMAND ${Python_EXECUTABLE} ${CMAKE_BINARY_DIR}/test.py
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
