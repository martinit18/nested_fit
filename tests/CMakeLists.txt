# NOTE(César): Testing requires python, I could run it off C++ or even Fortran, but I don't see the need to do so.
find_package(Python COMPONENTS Interpreter REQUIRED)

message(STATUS "Tests are enabled.")

# Read from the test script output and regex its value for "Test Passed" or something similar
set(passRegex "Test OK")
set(failRegex "Test FAIL")

# TODO(Cesar): Remake the python testing script to allow for a more precise testing environment

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

function(add_test_dir tname)
    # Is this a data or function run ?
    file(GLOB ALL_FILES ${tname}/*)
    list(LENGTH ALL_FILES FILECOUNT)

    if(FILECOUNT EQUAL 1)
        set(nf_test_bin ${nested_fit_target_func})
    else()
        set(nf_test_bin ${nested_fit_target})
    endif()

    # Are we using openMPI to test ?
    if(OPENMPI)
        set(nf_test_mt_type "OpenMPI")
        # If we are using OpenMPI use at least NCORES tries (?)
        cmake_host_system_information(RESULT NCORES QUERY NUMBER_OF_PHYSICAL_CORES)
    else()
        set(nf_test_mt_type "Baseline")
        # Else just set the number of tries to the default
        if(FILECOUNT EQUAL 1)
            set(NCORES 3) # 3 For functions
        else()
            set(NCORES 1) # 1 For data
        endif()
    endif()
    
    # How many tries should we use ?
    set(nf_test_current_ntries ${NCORES})
    
    message(STATUS "Configuring test -> ${tname} | ${nf_test_mt_type} | ${nf_test_bin} | ${NCORES} try/tries")

    add_test(
        NAME ${tname}
        COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/testing.py ${tname} ${nf_test_bin} 
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${tname}/
    )

    set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION "${passRegex}")
    set_property(TEST ${tname} PROPERTY FAIL_REGULAR_EXPRESSION "${failRegex}")

    # Configure test files version input
    configure_file(${tname}/nf_input.dat.in ${tname}/nf_input.dat @ONLY)
    file(COPY ${tname}/ 
         DESTINATION ${CMAKE_BINARY_DIR}/tests/${tname}/
         PATTERN "*.in" EXCLUDE
    )

    if(AUTOTESTS)
        add_custom_target(test_${tname} ALL
            COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIGURATION> -R "^${tname}$" --output-on-failures
        )
        add_dependencies(test_${tname} ${nested_fit_target} ${nested_fit_target_func})
    endif()
endfunction()

# Test the tesing suite itself
add_test(NAME expect COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/testing.py expect ${nested_fit_target}
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/expect/
)
set_property(TEST expect PROPERTY PASS_REGULAR_EXPRESSION "Test FAIL -> 3/4 passed.")


# Add test directories recursively
listsubdir(test_dirs ${CMAKE_CURRENT_SOURCE_DIR})
message(STATUS ${test_dirs})

# foreach(test_dir ${test_dirs})
#     add_test_dir(${test_dir})
# endforeach()
