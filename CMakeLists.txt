cmake_minimum_required(VERSION 3.10)

#####################################
# Custom variables are here for now #
#####################################

# Tests 
#enable_testing()
#add_subdirectory(tests)

# The latest nested_res -> This gets copied to the user bin/
set(nested_res_name nested_res_V4p5.py)

# The nested_fit version being built -> This gets copied everywhere inside the project code
# For more info see the 'src/Mod_metadata.f90.in' file
set(project_version 5.0.0)

# Global options
option(DEBUG "Enable debug mode." OFF)
option(NORNG "Uses the same set seed. Useful for testing." OFF)
option(OPENMP "Enable/Disable OpenMP." OFF)
option(OPENMPI "Enable/Disable OpenMPI." OFF)
option(AUTOTESTS "Automatically run tests after compiling." OFF)
option(LAPACK "Enable/Disable LAPACK." OFF)
option(BUILD_NESTED_PY "Automatically build nested_res for PyPI." OFF)
option(LTRACE "Enable trace logging." OFF)

if(OPENMPI AND WIN32)
    message(FATAL_ERROR "Windows builds don't currently support OpenMPI.")
endif()

if(LTRACE)
    add_definitions(-DLTRACE)
endif()

#####################################
#####################################

project(nested_fit VERSION ${project_version} LANGUAGES Fortran CXX)

#if(LAPACK)
#    find_package(BLAS REQUIRED)
#    find_package(LAPACK REQUIRED)
#endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(nested_fit_version_full_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}")
string(LENGTH ${nested_fit_version_full_str} nested_fit_version_full_str_len)

set(nested_fit_version_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}")
string(LENGTH ${nested_fit_version_str} nested_fit_version_str_len)

string(CONCAT nested_fit_mpi_child_comm_proc_name "nf_child_proc" ${nested_fit_version_full_str})
string(LENGTH ${nested_fit_mpi_child_comm_proc_name} nested_fit_mpi_child_comm_proc_name_len)

string(CONCAT nested_fit_target "nested_fit" ${nested_fit_version_full_str})
string(CONCAT nested_fit_target_func "nested_fit_func" ${nested_fit_version_full_str})
string(LENGTH ${nested_fit_target} nested_fit_target_len)
string(LENGTH ${nested_fit_target_func} nested_fit_target_func_len)

set(SRC_FILES_COMM
    src/Mod_cluster_analysis.f90
    src/Mod_parameters.f90
    src/Mod_search_new_point.f90
    src/Mod_timestamp.f90

    src/Mod_mpi.f90
    src/Mod_math.f90

    src/init_interpolation.f

    src/nested_fit.f90
    src/nested_sampling.f90

    src/rinteg.f
    src/shirley_fitpack.f90

    src/cli/argparse.f90
    src/Mod_options.f90
    
    src/funceval/auto_func.f90
    src/functions/WOFZ.f

    src/helper/hash.f90
    src/Mod_input_parse.f90

    src/Mod_userfcn.f90

    src/Mod_logger.f90

    src/helper/halt.f90
    src/helper/strutil.f90
)

set(SRC_EXTERNAL_FUNCS
    src/helper/hash.f90
    src/Mod_interpolate.f90
    src/functions/internal_funcs.f90
    src/Mod_logger.f90      # NOTE(César): Required for logging from the shared lib
    src/Mod_timestamp.f90   # NOTE(César): Required for logging from the shared lib
    src/functions/WOFZ.f
)

set(SRC_FILES_DATA
    src/Mod_likelihood.f90

    src/USERFCN_2D.f90
    src/USERFCN_SET.f
    src/USERFCN.f
)

set(SRC_FILES_FUNC
    src/Mod_likelihood_func.f90
)

set(CHILD_SRC_FILES
    src/helper/mpi_status_process.f90
    src/Mod_mpi.f90
)

file(GLOB SRC_DIERCKX ${CMAKE_SOURCE_DIR}/src/DIERCKX/*.f)
file(GLOB SRC_SLATEC  ${CMAKE_SOURCE_DIR}/src/SLATEC/*.f)
#file(GLOB SRC_LAPACK  ${CMAKE_SOURCE_DIR}/src/LAPACK/*.f ${CMAKE_SOURCE_DIR}/src/LAPACK/*.F)

if(OPENMP AND OPENMPI)
    message(ERROR " Compiling against OpenMP and OpenMPI at the same time is currently not supported.")
    message(WARNING "Disabling OpenMP.")
    message(STATUS "Please enable only one feature.")
    set(OPENMP OFF CACHE BOOL "" FORCE)
endif()

if(OPENMP AND NORNG)
    message(ERROR " Having a set seed while enabling OpenMP does not work.")
    message(WARNING "Disabling OpenMP.")
    message(STATUS "Please enable only one feature.")
    set(OPENMP OFF CACHE BOOL "" FORCE)
endif()

if(OPENMP)
    set(nested_fit_parallel_on ".TRUE.")
else()
    set(nested_fit_parallel_on ".FALSE.")
endif()

if(OPENMPI)
    set(nested_fit_parallel_mpi_on ".TRUE.")
else()
    set(nested_fit_parallel_mpi_on ".FALSE.")
endif()

if(NORNG)
    set(nested_fit_static_seed ".TRUE.")
else()
    set(nested_fit_static_seed ".FALSE.")
endif()

# Add the cache folder location (usually .nested_fit under $HOME Linux, and %appdata% for Windows)
if(WIN32)
    set(nested_fit_cache_folder $ENV{APPDATA}/.nested_fit/)
else()
    set(nested_fit_cache_folder $ENV{HOME}/.nested_fit/)
endif()
string(LENGTH ${nested_fit_cache_folder} nested_fit_cache_folder_len)
message(STATUS "Setting cache folder location: ${nested_fit_cache_folder}")

# Create the internal functions imutable cache config
set(Internal_CustomBuiltins "")
file(READ src/functions/internal_funcs.f90 internal_funcs_data)
string(REGEX MATCHALL "FUNCTION ([a-zA-Z0-9_]*)\\(" internal_funcs_declaration ${internal_funcs_data})
foreach(i ${internal_funcs_declaration})
    string(REGEX MATCH "FUNCTION ([a-zA-Z0-9_]*)\\(" dummy ${i})
    list(APPEND Internal_CustomBuiltins ${CMAKE_MATCH_1})
endforeach()
configure_file(src/funceval/latex_parser.cpp latex_parser.cpp @ONLY)

configure_file(src/Mod_metadata.f90.in Mod_metadata.f90)

set(CFG_FILES
    ${CMAKE_CURRENT_BINARY_DIR}/Mod_metadata.f90
)

set(SRC_FILES_CXX
    src/helper/disable_stdout.cpp
    src/funceval/native_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/latex_parser.cpp
)

add_executable(${nested_fit_target}
    ${CFG_FILES}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES_DATA}
    ${SRC_FILES_COMM}
    ${SRC_FILES_CXX}
)

add_executable(${nested_fit_target_func}
    ${CFG_FILES}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES_FUNC}
    ${SRC_FILES_COMM}
    ${SRC_FILES_CXX}
)

# TODO(César): This assumes we are using gfortran!
set(SRC_FLAGS_D     -cpp -O0 -Wall -g -ffree-line-length-0 -ffixed-line-length-0 -Wno-ampersand -Wno-tabs -Wno-unused-dummy-argument)
set(SRC_FLAGS_R     -cpp -O2 -static -ffree-line-length-0 -ffixed-line-length-0)

set(SRC_FLAGS_CXX_D -O0 -Wall -pedantic -g)      # Be pedantic
set(SRC_FLAGS_CXX_R -O2 -Wall -pedantic -static) # Be pedantic on release too


# NOTE(César): This is required to preprocessor the OpenMP comments
set(SRC_FLAGS_OMP "")
if(OPENMP)
    set(SRC_FLAGS_OMP -fopenmp)
endif()

target_compile_options(${nested_fit_target} PUBLIC
    $<$<COMPILE_LANGUAGE:Fortran>:$<IF:$<CONFIG:Debug>,${SRC_FLAGS_D},${SRC_FLAGS_R}>>
    $<$<COMPILE_LANGUAGE:CXX>:$<IF:$<CONFIG:Debug>,${SRC_FLAGS_CXX_D},${SRC_FLAGS_CXX_R}>>
    ${SRC_FLAGS_OMP}
)

target_compile_options(${nested_fit_target_func} PUBLIC 
    $<$<COMPILE_LANGUAGE:Fortran>:$<IF:$<CONFIG:Debug>,${SRC_FLAGS_D},${SRC_FLAGS_R}>>
    $<$<COMPILE_LANGUAGE:CXX>:$<IF:$<CONFIG:Debug>,${SRC_FLAGS_CXX_D},${SRC_FLAGS_CXX_R}>>
    ${SRC_FLAGS_OMP}
)

target_compile_definitions(${nested_fit_target_func} PRIVATE -DFUNC_TARGET)

if(LAPACK)
    target_compile_definitions(${nested_fit_target}      PUBLIC -DLAPACK_ON)
    target_compile_definitions(${nested_fit_target_func} PUBLIC -DLAPACK_ON)
    if(UNIX)
        # Find LAPACK and BLAS on Unix-like systems
        # old try set(SRC_FLAGS "${SRC_FLAGS} -L/usr/lib/x86_64-linux-gnu/liblapack.so -llapack -L/usr/lib/x86_64-linux-gnu/libblas.so -lblas")
        find_package(LAPACK REQUIRED)  
        if(LAPACK_FOUND)
            target_link_libraries(${nested_fit_target} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
            target_link_libraries(${nested_fit_target_func} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
        else()
            message(FATAL_ERROR "LAPACK library not found. Please install LAPACK.")
        endif()
    elseif(APPLE)
        find_library(ACCELERATE_FRAMEWORK Accelerate)
        target_link_libraries(${nested_fit_target}      PRIVATE ${ACCELERATE_FRAMEWORK})
        target_link_libraries(${nested_fit_target_func} PRIVATE ${ACCELERATE_FRAMEWORK})
    elseif(WIN32)
        # Specify LAPACK and BLAS paths and names on Windows
        #set(LAPACK_LIBRARIES "path/to/lapack.lib")
        #set(BLAS_LIBRARIES "path/to/blas.lib")
        #target_link_libraries(${nested_fit_target} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
        #target_link_libraries(${nested_fit_target_func} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
        message(FATAL_ERROR "LAPACK library not found. Please install LAPACK.")
    endif()
endif()

target_link_libraries(${nested_fit_target}      PUBLIC dl)
target_link_libraries(${nested_fit_target_func} PUBLIC dl)

if(OPENMP)
    target_link_options(${nested_fit_target}      PRIVATE "-fopenmp")
    target_link_options(${nested_fit_target_func} PRIVATE "-fopenmp")
endif()

if(NORNG)
    # Set the NORNG_ON flag for the preprocessor
    target_compile_definitions(${nested_fit_target}      PUBLIC -DNORNG_ON)
    target_compile_definitions(${nested_fit_target_func} PUBLIC -DNORNG_ON)
endif()

# Find MPI
if(OPENMPI)
    # Set the OPENMPI_ON flag for the preprocessor
    target_compile_definitions(${nested_fit_target}      PUBLIC -DOPENMPI_ON)
    target_compile_definitions(${nested_fit_target_func} PUBLIC -DOPENMPI_ON)

    # Add the required console writter (monitor) executable
    # This runs on a separate OpenMPI COMM
    add_executable(${nested_fit_mpi_child_comm_proc_name}
        ${CFG_FILES}
        ${CHILD_SRC_FILES}
    )

    find_package(MPI REQUIRED)
    if(MPI_FOUND)
        if(MPI_Fortran_HAVE_F90_MODULE)
            target_link_libraries(${nested_fit_target}                   PUBLIC MPI::MPI_Fortran)
            target_link_libraries(${nested_fit_target_func}              PUBLIC MPI::MPI_Fortran)
            target_link_libraries(${nested_fit_mpi_child_comm_proc_name} PUBLIC MPI::MPI_Fortran)
        else()
            message(FATAL_ERROR "This project requires the MPI Fortran 90 module.")
        endif()
    else()
        message(FATAL_ERROR "OpenMPI option specified, but could not find the MPI package.")
    endif()
endif()

# Create the cache folder at build time
add_custom_command(TARGET ${nested_fit_target}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${nested_fit_cache_folder}
)

# Compile the object files of the internal functions the user will be able to natively use in their expressions
#TODO(César): Use generator expression for compile flags here too
#TODO(César): Optimize fPIC with https://www.macieira.org/blog/2012/01/sorry-state-of-dynamic-libraries-on-linux/
set_source_files_properties(${SRC_DIERCKX} PROPERTIES COMPILE_FLAGS "-fPIC -fno-semantic-interposition -static -O2 -ffree-line-length-0")
set_source_files_properties(${SRC_EXTERNAL_FUNCS} PROPERTIES COMPILE_FLAGS "-fPIC -fno-semantic-interposition -static -O2 -Wall -ffree-line-length-0")
add_library(internal_functions OBJECT ${SRC_EXTERNAL_FUNCS} ${SRC_DIERCKX})
add_custom_target(copy_internal_functions ALL
    DEPENDS internal_functions
    COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_OBJECTS:internal_functions> ${nested_fit_cache_folder}
    COMMAND_EXPAND_LISTS
)

if(BUILD_NESTED_PY)
    find_package(Python COMPONENTS Interpreter QUIET)
    set(nested_res_dist_dir ${CMAKE_BINARY_DIR}/nested_py)
    if(Python_FOUND)
        # Upgrade the PyPA's build
        install(CODE
            "# add_custom_command(TARGET ${nested_fit_target} POST_BUILD
            execute_process(
                COMMAND ${Python_EXECUTABLE} -m pip install --upgrade build
            )"
        )

        # Just build the package
        install(CODE
            "# add_custom_command(TARGET ${nested_fit_target} POST_BUILD
            execute_process(
                COMMAND ${Python_EXECUTABLE} -m build ${CMAKE_SOURCE_DIR}/nested_py/ --outdir ${nested_res_dist_dir}
            )"
        )

        # No need to build from wheel (we got the source)
        install(CODE "execute_process(COMMAND ${Python_EXECUTABLE} -m pip install ${CMAKE_SOURCE_DIR}/nested_py/)")

        # Make a custom target for python also, not requiring installation
        add_custom_target(nested_py
            ${Python_EXECUTABLE} -m pip install --upgrade build
            COMMAND ${Python_EXECUTABLE} -m build ${CMAKE_SOURCE_DIR}/nested_py/ --outdir ${nested_res_dist_dir}
            BYPRODUCTS *.whl
        )

        # Add a custom target for publishing python to the online PyPI index
        # TODO(César)
    else()
        message(WARNING "Python not found. nested_res will not be built/installed on the system.")
        message(WARNING "If you wish to build, install python3 and re-run this command.")
    endif()
endif()

install(TARGETS ${nested_fit_target}      DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
install(TARGETS ${nested_fit_target_func} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)

if(OPENMPI)
    # Do the same for the child process in case it was compiled
    install(TARGETS ${nested_fit_mpi_child_comm_proc_name} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
endif()

# enable_testing() Don't use CTest
add_subdirectory(tests)
