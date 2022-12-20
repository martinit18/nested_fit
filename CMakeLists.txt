cmake_minimum_required(VERSION 3.16)

#####################################
# Custom variables are here for now #
#####################################

# The latest nested_res -> This gets copied to the user bin/
set(nested_res_name nested_res_V4p2.py)

# The nested_fit version being built -> This gets copied everywhere inside the project code
# For more info see the 'src/Mod_metadata.f90.in' file
set(project_version 4.3.0)

# Global options
option(DEBUG "Enable debug mode." OFF)
option(NORNG "Uses the same set seed. Useful for testing." OFF)
# option(MOD_TEST "Uses another likelihood function." OFF)
option(OPENMP "Enable/Disable OpenMP." OFF) # This replaced the manual parallel_on variable on the nested_fit.f90 file
option(OPENMPI "Enable/Disable OpenMPI." ON)
option(AUTOTESTS "Automatically run tests after compiling." OFF)

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


# if(MOD_TEST)
#     set(mod_lh_test _tests)
# endif()

set(SRC_FILES
    src/Mod_cluster_analysis.f90
    # src/Mod_likelihood_tests.f90
    src/Mod_likelihood${mod_lh_test}.f90
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

    src/USERFCN_2D.f90
    src/USERFCN_SET.f
    src/USERFCN.f

    src/Mod_mpi.f90
)

set(CHILD_SRC_FILES
    src/helper/mpi_status_process.f90
    src/Mod_mpi.f90
)

file(GLOB SRC_CALGO   ${CMAKE_SOURCE_DIR}/src/CALGO.ACM/*.f)
file(GLOB SRC_DIERCKX ${CMAKE_SOURCE_DIR}/src/DIERCKX/*.f)
file(GLOB SRC_SLATEC ${CMAKE_SOURCE_DIR}/src/SLATEC/*.f)

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

# HACK(César): This is assuming we are using gfortran!
if(DEBUG)
    message(STATUS "Configuring for Debug")
    set(SRC_FLAGS "-O0 -g -ffree-line-length-0")
else()
    message(STATUS "Configuring for Release")
    set(SRC_FLAGS "-O3 -static -ffree-line-length-0")

    # NOTE(César): Enable OpenMP only on release (Assuming we want to debug one thread only for now).
    # TODO(César): OpenMP should be a link only flag, no?
    if(OPENMP)
        set(SRC_FLAGS "${SRC_FLAGS} -fopenmp")
    endif()
endif()

set_source_files_properties(${SRC_CALGO} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_DIERCKX} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_SLATEC} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${SRC_FILES} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${CFG_FILES} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})
set_source_files_properties(${CHILD_SRC_FILES} PROPERTIES COMPILE_FLAGS ${SRC_FLAGS})

string(CONCAT nested_fit_target "nested_fit" ${nested_fit_version_full_str})
add_executable(${nested_fit_target}
    ${CFG_FILES}
    ${SRC_CALGO}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES}
)

if(OPENMPI)
    add_executable(${nested_fit_mpi_child_comm_proc_name}
        ${CFG_FILES}
        ${CHILD_SRC_FILES}
    )
endif()

if(OPENMP)
    target_link_options(${nested_fit_target} PRIVATE "-fopenmp")
endif()

# TODO(César): MPI is needed even for non-MPI builds, since the source code is runtime selecting mpi funcitonality based on a PARAMETER
# This needs a fix!!!!
# Find MPI
find_package(MPI REQUIRED)
if(MPI_FOUND)
    if(MPI_Fortran_HAVE_F90_MODULE)
        target_link_libraries(${nested_fit_target} PUBLIC MPI::MPI_Fortran)
        if(OPENMPI)
            target_link_libraries(${nested_fit_mpi_child_comm_proc_name} PUBLIC MPI::MPI_Fortran)
        endif()
    else()
        message(ERROR "This project requires the MPI Fortran 90 module.")
    endif()
else()
    message(ERROR "OpenMPI option specified, but could not find the MPI package.")
endif()

# NOTE(César): Don't run these commands for the Windows build.
if(WIN32)
    # TODO(César): Create a automatic configuration for Windows as well
    message(STATUS "Configuring for Windows. The executable and python library must be manually copied somewhere and added to the PATH.")
else()
    # Install would work well here but lets keep the same copy-after-build scheme of the original makefile
    # TODO(César): Maybe use install later on
    add_custom_command(TARGET ${nested_fit_target} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${nested_fit_target}> $ENV{HOME}/bin)

    # Since we do this for the nested fit executable, why not put the latest nested_res_* file in the bin
    # At least while we don't have a custom python caller script
    add_custom_command(TARGET ${nested_fit_target} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/${nested_res_name} $ENV{HOME}/bin)

    if(OPENMPI)
        # Do the same for the child process in case it was compiled
        add_custom_command(TARGET ${nested_fit_mpi_child_comm_proc_name} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${nested_fit_mpi_child_comm_proc_name}> $ENV{HOME}/bin)
    endif()
endif()

enable_testing()
add_subdirectory(tests)
