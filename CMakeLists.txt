cmake_minimum_required(VERSION 3.10)

#####################################
# This CMake file also generates an #
# autoconf file alongside an auto   #
# make one. This is done to support #
# backwards compatibility for older #
# systems.                          #
#####################################
set(AUTO_MAKE_INLINE_CODE "")
set(AUTO_MAKE_INLINE_CODE_PRE "")

macro(ac_dec_obj_dep name deps)
    string(APPEND AUTO_MAKE_INLINE_CODE "${name}.o: ${deps}.o\n")
endmacro(ac_dec_obj_dep)

macro(ac_inline_generic code)
    string(APPEND AUTO_MAKE_INLINE_CODE_PRE "${code}\n")
endmacro(ac_inline_generic)
#####################################
#####################################

# The nested_fit version being built -> This gets copied everywhere inside the project code
# For more info see the 'src/Mod_metadata.f90.in' file
# Read the version from the .toml file since this is what will be pushed to the repo
file(READ "pyproject.toml" tomlfile)
string(REGEX MATCH "version = \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" vn ${tomlfile})
set(project_version ${CMAKE_MATCH_1})

# This is just for the clangd LSP
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Global options
option(DEBUG "Enable debug mode." OFF)
option(NORNG "Uses the same set seed. Useful for testing." OFF)
option(OPENMP "Enable/Disable OpenMP." OFF)
option(OPENMPI "Enable/Disable OpenMPI." OFF)
option(AUTOTESTS "Automatically run tests after compiling." OFF)
option(LAPACK "Enable/Disable LAPACK." OFF)
option(LTRACE "Enable trace logging." OFF)
option(PPROF "Enable performance counter profilling." OFF)
option(INSTALL_SYSTEM_WIDE "Install on '/usr/local' instead of '$HOME/.local'." OFF) # Requires privileged user
option(CREATE_AUTOCONF "Builds a 'configure' script for older systems." OFF)
set(NF_CACHE_DIR "" CACHE STRING "Sets a custom cache directory for nested_fit.")

if(OPENMPI AND WIN32)
    message(FATAL_ERROR "Windows builds don't currently support OpenMPI.")
endif()

if(LTRACE)
    add_definitions(-DLTRACE)
endif()

#####################################
#####################################

set(project_name nested_fit)
project(${project_name} VERSION ${project_version} LANGUAGES Fortran CXX)

# Enforce C++14
set(CMAKE_CXX_STANDARD 14)

#if(LAPACK)
#    find_package(BLAS REQUIRED)
#    find_package(LAPACK REQUIRED)
#endif()

# Performance profiling
add_definitions(-DPPROF)
include(FetchContent)
FetchContent_Declare(
    tracy
    GIT_REPOSITORY https://github.com/wolfpld/tracy.git
    GIT_TAG v0.10
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
)

# NOTE: (César) : MakeAvailable only supports EXCLUDE_FROM_ALL after
#                 cmake 3.28, so we add this workaround for 3.10 compat
#                 FetchContent_MakeAvailable(tracy)
FetchContent_GetProperties(tracy)
if(NOT tracy_POPULATED)
  FetchContent_Populate(tracy)
  add_subdirectory(${tracy_SOURCE_DIR} ${tracy_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()

if(PPROF)
    add_definitions(-DTRACY_ENABLE -DTRACY_NO_EXIT)
endif()

# BUG: (César) This is probably a GNU only way of defining macros from the CLI
add_definitions("-D'PASTE(x)'='x' -D'CAT(x,y)'='PASTE(x)y'")
# add_definitions("-D'PROFILED(x)'='TYPE(ScopedPerfTimer) :: CAT(x,__LINE__); CALL CAT(x,__LINE__)%init(\"x\",__LINE__,__FILE__);'")

add_definitions("-D'PROFILED(x)'=''")

if(NOT INSTALL_SYSTEM_WIDE)
    if(WIN32)
        # On Windows, install on the user folder under 'nested_fit'
        set(CMAKE_INSTALL_PREFIX $ENV{USERPROFILE}/nested_fit)
    else()
        # On Linux/MacOS install on the home/.local
        set(CMAKE_INSTALL_PREFIX $ENV{HOME}/.local)
    endif()
endif()
message(STATUS "Install prefix set to: ${CMAKE_INSTALL_PREFIX}") 

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(nested_fit_version_full_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}")

set(nested_fit_version_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}")

string(CONCAT nested_fit_mpi_child_comm_proc_name "nf_child_proc" ${nested_fit_version_full_str})

string(CONCAT nested_fit_target "nested_fit" ${nested_fit_version_full_str})
string(CONCAT nested_fit_target_func "nested_fit_func" ${nested_fit_version_full_str})

string(REPLACE "." "_" nested_fit_target_ac ${nested_fit_target})
string(REPLACE "." "_" nested_fit_target_func_ac ${nested_fit_target_func})

set(SRC_FILES_COMM
    src/Mod_cluster_analysis.f90
    src/Mod_parameters.f90
    src/Mod_covariance_matrix.f90
    src/Mod_search_new_point.f90    #
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
    src/Mod_jsonio.f90

    src/Mod_userfcn.f90

    src/Mod_logger.f90

    src/Mod_perfprof.f90

    src/helper/halt.f90
    src/helper/strutil.f90
    src/helper/int_stack.f90
)
string(REPLACE ";" " " SRC_FILES_COMM_AC "${SRC_FILES_COMM}")
ac_dec_obj_dep("src/Mod_cluster_analysis" "src/Mod_logger")
ac_dec_obj_dep("src/Mod_logger" "src/Mod_timestamp")
ac_dec_obj_dep("src/Mod_cluster_analysis" "src/Mod_parameters")
ac_dec_obj_dep("src/Mod_search_new_point" "src/Mod_likelihood")
ac_dec_obj_dep("src/Mod_search_new_point" "src/Mod_perfprof") # Performance profilling is not compatible with autoconf
ac_dec_obj_dep("src/Mod_likelihood" "src/Mod_userfcn")
ac_dec_obj_dep("src/Mod_userfcn" "src/funceval/auto_func")
ac_dec_obj_dep("src/funceval/auto_func" "src/Mod_metadata")
ac_dec_obj_dep("src/funceval/auto_func" "src/helper/strutil")
ac_dec_obj_dep("src/funceval/auto_func" "src/Mod_options")
ac_dec_obj_dep("src/Mod_likelihood" "src/Mod_math")
ac_dec_obj_dep("src/nested_fit" "src/Mod_input_parse")
ac_dec_obj_dep("src/Mod_input_parse" "src/helper/int_stack")
ac_dec_obj_dep("src/Mod_jsonio" "src/Mod_logger")
ac_dec_obj_dep("src/nested_fit" "src/cli/argparse")
ac_dec_obj_dep("src/nested_fit" "src/Mod_jsonio")

set(SRC_EXTERNAL_FUNCS
    src/helper/hash.f90
    src/Mod_interpolate.f90
    src/functions/internal_funcs.f90
    src/Mod_logger.f90      # NOTE :(César) Required for logging from the shared lib
    src/Mod_timestamp.f90   # NOTE :(César) Required for logging from the shared lib
    src/functions/WOFZ.f
)
set(SRC_EXTERNAL_FUNCS_AC_F90 "")
set(SRC_EXTERNAL_FUNCS_AC_F77 "")

foreach(file ${SRC_EXTERNAL_FUNCS})
    get_filename_component(file_ext ${file} EXT)
    string(REGEX REPLACE "\\.[^.]*$" "" file_noext ${file})

    if(${file_ext} STREQUAL ".f")
        set(SRC_EXTERNAL_FUNCS_AC_F77 "${SRC_EXTERNAL_FUNCS_AC_F77} ${file_noext}.o")
    else()
        set(SRC_EXTERNAL_FUNCS_AC_F90 "${SRC_EXTERNAL_FUNCS_AC_F90} ${file_noext}.o")
    endif()

    message(STATUS "file -> ${file} | ${file_noext}.o")
endforeach()

ac_dec_obj_dep("src/Mod_interpolate" "src/Mod_logger")
ac_dec_obj_dep("src/functions/internal_funcs" "src/Mod_logger")
# ac_dec_obj_dep("src/functions/internal_funcs" "src/hash")

set(SRC_FILES_DATA
    src/Mod_likelihood.f90

    src/USERFCN_2D.f90
    src/USERFCN_SET.f
    src/USERFCN.f
)
string(REPLACE ";" " " SRC_FILES_DATA_AC "${SRC_FILES_DATA}")

set(SRC_FILES_FUNC
    src/Mod_likelihood_func.f90
)
string(REPLACE ";" " " SRC_FILES_FUNC_AC "${SRC_FILES_FUNC}")

set(CHILD_SRC_FILES
    src/helper/mpi_status_process.f90
    src/Mod_mpi.f90
)

file(GLOB SRC_DIERCKX RELATIVE ${CMAKE_SOURCE_DIR} src/DIERCKX/*.f)
string(REPLACE ";" " " SRC_DIERCKX_AC "${SRC_DIERCKX}")

file(GLOB SRC_SLATEC  RELATIVE ${CMAKE_SOURCE_DIR} src/SLATEC/*.f)
string(REPLACE ";" " " SRC_SLATEC_AC "${SRC_SLATEC}")

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
if(NF_CACHE_DIR STREQUAL "")
    if(WIN32)
        set(nested_fit_cache_folder $ENV{APPDATA}/.nested_fit/ CACHE STRING "")
    else()
        set(nested_fit_cache_folder $ENV{HOME}/.nested_fit/ CACHE STRING "")
    endif()
else()
    set(nested_fit_cache_folder ${NF_CACHE_DIR} CACHE STRING "")
endif()
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

configure_file(src/Mod_metadata.f90.in Mod_metadata.f90 @ONLY)

set(CFG_FILES
    ${CMAKE_CURRENT_BINARY_DIR}/Mod_metadata.f90
)

set(SRC_FILES_CXX
    src/helper/console.cpp
    src/helper/disable_stdout.cpp
    src/funceval/native_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/latex_parser.cpp
)
set(SRC_FILES_CXX_AC
    "src/helper/console.cpp src/helper/disable_stdout.cpp src/funceval/latex_parser.cpp src/funceval/native_parser.cpp"
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

set(SRC_FLAGS_CXX_D -O0 -Wall -pedantic -g -Wno-missing-field-initializers ${CXX_SUPPRESS_STPROF})      # Be pedantic
set(SRC_FLAGS_CXX_R -O2 -Wall -pedantic -static -Wno-missing-field-initializers ${CXX_SUPPRESS_STPROF}) # Be pedantic on release too

# NOTE(César): Autoconf will use release only for now
#              (this is semi ok, since it is not intended to be a used in a dev env)
#              This behaviour is basically just inside configure.ac.in
string(REPLACE ";" " " AC_FLAGS_CXX "${SRC_FLAGS_CXX_R}")
string(REPLACE ";" " " AC_FLAGS_CF  "${SRC_FLAGS_R}")
ac_inline_generic("CPPFLAGS = ${AC_FLAGS_CXX}")
ac_inline_generic("FCFLAGS = ${AC_FLAGS_CF}")
ac_inline_generic("FFLAGS = ${AC_FLAGS_CF}")

# NOTE: (César) This approach does not work for windows (but autoconf does not either)
#               Also assuming gfortran here
ac_inline_generic("LIBS = -lgfortran -ldl")

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

target_include_directories(${nested_fit_target}      PRIVATE ${tracy_SOURCE_DIR})
target_include_directories(${nested_fit_target_func} PRIVATE ${tracy_SOURCE_DIR})

target_link_libraries(${nested_fit_target}      PRIVATE TracyClient)
target_link_libraries(${nested_fit_target_func} PRIVATE TracyClient)

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
    PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${nested_fit_cache_folder}
)
add_custom_command(TARGET ${nested_fit_target}
    PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${nested_fit_cache_folder}user/
)
add_custom_command(TARGET ${nested_fit_target}
    PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${nested_fit_cache_folder}sys/
)

# Compile the object files of the internal functions the user will be able to natively use in their expressions
#TODO(César): Use generator expression for compile flags here too
#TODO(César): Optimize fPIC with https://www.macieira.org/blog/2012/01/sorry-state-of-dynamic-libraries-on-linux/
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(SRC_DIERCKX_OBJ_FLAGS "-fPIC -fno-semantic-interposition -static -O3 -ffree-line-length-0")
else()
    set(SRC_DIERCKX_OBJ_FLAGS "-fPIC -fno-semantic-interposition -static -g -O0 -ffree-line-length-0")
endif()
set_source_files_properties(${SRC_DIERCKX} PROPERTIES COMPILE_FLAGS "${SRC_DIERCKX_OBJ_FLAGS}")

if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(SRC_EXTERNAL_FUNCS_OBJ_FLAGS "-fPIC -fno-semantic-interposition -static -O3 -Wall -ffree-line-length-0")
else()
    set(SRC_EXTERNAL_FUNCS_OBJ_FLAGS "-fPIC -fno-semantic-interposition -static -O0 -g -Wall -ffree-line-length-0")
endif()
set_source_files_properties(${SRC_EXTERNAL_FUNCS} PROPERTIES COMPILE_FLAGS "${SRC_EXTERNAL_FUNCS_OBJ_FLAGS}")

add_library(internal_functions OBJECT ${SRC_EXTERNAL_FUNCS} ${SRC_DIERCKX})
add_custom_target(copy_internal_functions ALL
    DEPENDS internal_functions
    COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_OBJECTS:internal_functions> ${nested_fit_cache_folder}sys/
    COMMAND_EXPAND_LISTS
)

install(TARGETS ${nested_fit_target}      DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
install(TARGETS ${nested_fit_target_func} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)

if(OPENMPI)
    # Do the same for the child process in case it was compiled
    install(TARGETS ${nested_fit_mpi_child_comm_proc_name} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
endif()

# configure compatibility files for autoconf / automake
if(CREATE_AUTOCONF)
    message(STATUS "Generated the following dependecies:")
    message(STATUS "\n${AUTO_MAKE_INLINE_CODE}")
    configure_file(configure.ac.in ${CMAKE_SOURCE_DIR}/configure.ac @ONLY)
    configure_file(Makefile.am.in ${CMAKE_SOURCE_DIR}/Makefile.am @ONLY)

    file(WRITE ${CMAKE_SOURCE_DIR}/NEWS "")
    file(WRITE ${CMAKE_SOURCE_DIR}/COPYING "")
    file(WRITE ${CMAKE_SOURCE_DIR}/ChangeLog "")
    file(WRITE ${CMAKE_SOURCE_DIR}/README "")
    file(WRITE ${CMAKE_SOURCE_DIR}/AUTHORS "")
    
    execute_process(COMMAND autoreconf --verbose --install --force WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    
    file(REMOVE ${CMAKE_SOURCE_DIR}/INSTALL)
    file(REMOVE ${CMAKE_SOURCE_DIR}/NEWS)
    file(REMOVE ${CMAKE_SOURCE_DIR}/COPYING)
    file(REMOVE ${CMAKE_SOURCE_DIR}/ChangeLog)
    file(REMOVE ${CMAKE_SOURCE_DIR}/README)
    file(REMOVE ${CMAKE_SOURCE_DIR}/AUTHORS)

    # Remove autoconf stuff from src tree
    file(REMOVE ${CMAKE_SOURCE_DIR}/aclocal.m4)
    # file(REMOVE ${CMAKE_SOURCE_DIR}/configure.ac)
    # file(REMOVE ${CMAKE_SOURCE_DIR}/Makefile.am)
endif()
enable_testing()
add_subdirectory(tests)
