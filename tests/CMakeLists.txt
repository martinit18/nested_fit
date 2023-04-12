# NOTE(César): Testing requires python, I could run it off C++ or even Fortran, but I don't see the need to do so.
find_package(Python COMPONENTS Interpreter REQUIRED)

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

macro(try_implement_json_field)
    set(options "")
    set(single_args JSON_DATA PARAMETER_OUT)
    set(multi_args KEYS)
    cmake_parse_arguments(ARGS "${options}" "${single_args}" "${multi_args}" ${ARGN})

    string(JSON var ERROR_VARIABLE var_found GET ${ARGS_JSON_DATA} ${ARGS_KEYS})
    if(${var_found} MATCHES "NOTFOUND")
        list(GET ARGS_KEYS -1 last_key)
        message(STATUS "Using custom key [${last_key}=${var}]")
        set(nf_test_filename_suffix ${nf_test_filename_suffix}_${var})
        set(${ARGS_PARAMETER_OUT} ${var})
    endif()
endmacro()

macro(add_test_dir tname isdata)
    # Are we using openMPI to test ?
    if(OPENMPI)
        set(nf_test_mt_type "OpenMPI")
        # If we are using OpenMPI use at least NCORES tries (?)
        cmake_host_system_information(RESULT NCORES QUERY NUMBER_OF_PHYSICAL_CORES)
    else()
        set(nf_test_mt_type "Baseline")
        # Else just set the number of tries to the default
        if(NOT ${isdata})
            set(NCORES 3) # 3 For functions
        else()
            set(NCORES 1) # 1 For data
        endif()
    endif()
    
    # How many tries should we use ?
    set(nf_test_current_ntries ${NCORES})
    message(STATUS "Configuring test -> ${tname} | ${nf_test_mt_type} | ${NCORES} try/tries")
endmacro()

macro(generate_test_subfolders ttype)
    # This is a data test directory
    listsubdir(data_test_dirs ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}/)

    foreach(data_test ${data_test_dirs})
        if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}/${data_test}/modes.json)

            # Tell cmake what are the keys in the json file to fetch for
            execute_process(COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/configure_json.py ${CMAKE_CURRENT_BINARY_DIR} ${data_test} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}/${data_test})
            include(${CMAKE_CURRENT_BINARY_DIR}/configure_json_keys_${data_test}.cmake)
            
            file(READ ${CMAKE_CURRENT_SOURCE_DIR}/${ttype}/${data_test}/modes.json json_data_str)
            
            # Set the variables for the input file config
            string(JSON variants ERROR_VARIABLE var_found GET ${json_data_str} "variants" 0 "search_func")
            if(DEFINED variants)
                string(JSON variants_length LENGTH ${json_data_str} "variants")
                MATH(EXPR variants_loop_idx "${variants_length}-1")
                
                foreach(i RANGE ${variants_loop_idx})
                    load_json_variants(${json_data_str} ${i})
                    message(STATUS "Generating folder: ${data_test}${nf_test_filename_suffix}")

                    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/tests/${data_test}${nf_test_filename_suffix}/)
                    list(APPEND nf_test_folders ${data_test}${nf_test_filename_suffix})

                    if(${ttype} STREQUAL "data")
                        add_test_dir(${data_test} TRUE)
                        list(APPEND nf_test_bin ${nested_fit_target})
                    else()
                        add_test_dir(${data_test} FALSE)
                        list(APPEND nf_test_bin ${nested_fit_target_func})
                    endif()

                    configure_file(${ttype}/${data_test}/nf_input.dat.in ${data_test}${nf_test_filename_suffix}/nf_input.dat @ONLY)
                    file(COPY ${ttype}/${data_test}/
                        DESTINATION ${CMAKE_BINARY_DIR}/tests/${data_test}${nf_test_filename_suffix}/
                        PATTERN "*.in"   EXCLUDE
                        PATTERN "*.json" EXCLUDE
                    )
                    message(STATUS "")
                    set(nf_test_filename_suffix "")
                endforeach()
            endif()
        else()
            message(STATUS "Generating folder: ${data_test}")

            if(${ttype} STREQUAL "data")
                add_test_dir(${data_test} TRUE)
                list(APPEND nf_test_bin ${nested_fit_target})
            else()
                add_test_dir(${data_test} FALSE)
                list(APPEND nf_test_bin ${nested_fit_target_func})
            endif()

            file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/tests/${data_test}/)
            list(APPEND nf_test_folders ${data_test})


            configure_file(${ttype}/${data_test}/nf_input.dat.in ${data_test}${nf_test_filename_suffix}/nf_input.dat @ONLY)
            file(COPY ${ttype}/${data_test}/
                DESTINATION ${CMAKE_BINARY_DIR}/tests/${data_test}${nf_test_filename_suffix}/
                PATTERN "*.in"   EXCLUDE
                PATTERN "*.json" EXCLUDE
            )
            message(STATUS "")
        endif()
    endforeach()
endmacro()

set(nf_test_bin "")
set(nf_test_folders "")

# Make the json loading part of the code available to the cmake generated modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/tests/)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/TryJsonField.cmake DESTINATION ${CMAKE_BINARY_DIR}/tests/)

# Finaly generate all of the test folders
generate_test_subfolders("data")
generate_test_subfolders("func")

# Copy the test suite file
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test_suite.py DESTINATION ${CMAKE_BINARY_DIR}/tests/)

# Configure the python test file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test.py ${CMAKE_BINARY_DIR}/test.py)
