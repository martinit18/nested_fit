# NOTE(César): Testing requires python, I could run it off C++ or even Fortran, but I don't see the need to do so.
find_package(Python COMPONENTS Interpreter REQUIRED)

message(STATUS "Tests are enabled.")

# Read from the test script output and regex its value for "Test Passed" or something similar
set(passRegex "Test OK")
set(failRegex "Test FAIL")

function(add_test_dir tname)
    add_test(
        NAME ${tname}
        COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/testing.py ${tname} ${nested_fit_target} 
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${tname}/
    )

    set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION "${passRegex}")
    set_property(TEST ${tname} PROPERTY FAIL_REGULAR_EXPRESSION "${failRegex}")

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


# Add test directories
add_test_dir(4gauss_bg)
add_test_dir(gauss_bg)
