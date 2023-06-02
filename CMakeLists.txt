cmake_minimum_required(VERSION 3.20)

#####################################
# Custom variables are here for now #
#####################################

# The nested_fit version being built -> This gets copied everywhere inside the project code
# For more info see the 'src/Mod_metadata.f90.in' file
set(project_version 4.5.1)

# Global options
option(DEBUG "Enable debug mode." OFF)
option(NORNG "Uses the same set seed. Useful for testing." OFF)
option(OPENMP "Enable/Disable OpenMP." OFF)
option(OPENMPI "Enable/Disable OpenMPI." OFF)
option(AUTOTESTS "Automatically run tests after compiling." OFF)
option(BUILD_NESTED_PY "Automatically build nested_res for PyPI." ON)

if(OPENMPI AND WIN32)
    message(FATAL_ERROR "Windows builds don't currently support OpenMPI.")
endif()

#####################################
#####################################

project(nested_fit VERSION ${project_version} LANGUAGES Fortran)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(nested_fit_version_full_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}")
string(LENGTH ${nested_fit_version_full_str} nested_fit_version_full_str_len)

set(nested_fit_version_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}")
string(LENGTH ${nested_fit_version_str} nested_fit_version_str_len)

string(CONCAT nested_fit_mpi_child_comm_proc_name "nf_child_proc" ${nested_fit_version_full_str})
string(LENGTH ${nested_fit_mpi_child_comm_proc_name} nested_fit_mpi_child_comm_proc_name_len)


set(SRC_FILES_COMM
    src/Mod_cluster_analysis.f90
    src/Mod_parameters.f90
    src/Mod_search_new_point.f90
    src/Mod_timestamp.f90
    
    src/dlog_fac.f90
    src/meanvar.f90
    src/init_interpolation.f

    src/nested_fit.f90
    src/nested_sampling.f90

    src/randn.f90
    src/rinteg.f
    src/shirley_fitpack.f90
    src/sortn_slatec.f90

    src/Mod_mpi.f90
    src/Mod_math.f90

    src/cli/f90getopt.f90
    src/Mod_options.f90
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

file(GLOB SRC_CALGO   ${CMAKE_SOURCE_DIR}/src/CALGO.ACM/*.f)
file(GLOB SRC_DIERCKX ${CMAKE_SOURCE_DIR}/src/DIERCKX/*.f)
file(GLOB SRC_SLATEC  ${CMAKE_SOURCE_DIR}/src/SLATEC/*.f)

if(OPENMP AND OPENMPI)
    message(ERROR "Compiling against OpenMP and OpenMPI at the same time is currently not supported.")
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

configure_file(src/Mod_metadata.f90.in Mod_metadata.f90)

set(CFG_FILES
    ${CMAKE_CURRENT_BINARY_DIR}/Mod_metadata.f90
)

# TODO(César): This assumes we are using gfortran!
# TODO(César): Use generator expressions instead!
if(DEBUG)
    message(STATUS "Configuring for Debug")
    set(SRC_FLAGS "-cpp -O0 -Wall -g -ffree-line-length-0")
else()
    message(STATUS "Configuring for Release")
    set(SRC_FLAGS "-cpp -O2 -static -ffree-line-length-0")
endif()

# NOTE(César): This is required to preprocessor the OpenMP comments
if(OPENMP)
    set(SRC_FLAGS "${SRC_FLAGS} -fopenmp")
endif()

set_source_files_properties(${SRC_CALGO} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_DIERCKX} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_SLATEC} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_FILES_COMM} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_FILES_DATA} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_FILES_FUNC} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${CFG_FILES} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${CHILD_SRC_FILES} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})

string(CONCAT nested_fit_target "nested_fit" ${nested_fit_version_full_str})
string(CONCAT nested_fit_target_func "nested_fit_func" ${nested_fit_version_full_str})

add_executable(${nested_fit_target}
    ${CFG_FILES}
    ${SRC_CALGO}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES_COMM}
    ${SRC_FILES_DATA}
)

add_executable(${nested_fit_target_func}
    ${CFG_FILES}
    ${SRC_CALGO}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES_COMM}
    ${SRC_FILES_FUNC}
)

if(OPENMP)
    target_link_options(${nested_fit_target}      PRIVATE "-fopenmp")
    target_link_options(${nested_fit_target_func} PRIVATE "-fopenmp")
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


if(BUILD_NESTED_PY)
    find_package(Python COMPONENTS Interpreter QUIET)
    set(nested_res_dist_dir ${CMAKE_BINARY_DIR}/nested_py)
    if(Python_FOUND)
        # Upgrade the PyPA's build
        add_custom_command(TARGET ${nested_fit_target} POST_BUILD
            COMMAND ${Python_EXECUTABLE} -m pip install --upgrade build
        )
        # Just build the package
        add_custom_command(TARGET ${nested_fit_target} POST_BUILD
            COMMAND ${Python_EXECUTABLE} -m build ${CMAKE_SOURCE_DIR}/nested_py/ --outdir ${nested_res_dist_dir}
        )

        # No need to build from wheel (we got the source)
        install(CODE "execute_process(COMMAND ${Python_EXECUTABLE} -m pip install ${CMAKE_SOURCE_DIR}/nested_py/)")

        # Add a custom target for publishing python to the online PyPI index
        # TODO
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

enable_testing()
add_subdirectory(tests)
