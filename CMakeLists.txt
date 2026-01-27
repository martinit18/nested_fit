cmake_minimum_required(VERSION 3.10)

# NOTE: (César) : If we are on macOS set the minimum supported version to 10.15
#				  due to C++17 support.
# if(APPLE)
# 	set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING "" FORCE)
# endif()

# The nested_fit version being built -> This gets copied everywhere inside the project code
# For more info see the 'src/Mod_metadata.f90.in' file
# Read the version from the .toml file since this is what will be pushed to the repo
file(READ "pyproject.toml" tomlfile)
string(REGEX MATCH "version[ \t]*=[ \t]*\"([0-9]+\.[0-9]+\.[0-9]+)(\.dev[0-9]+)?\"" vn ${tomlfile})
set(project_version ${CMAKE_MATCH_1})
message(STATUS "Build: ${CMAKE_MATCH_1}${CMAKE_MATCH_2}")
set(dev_version "${CMAKE_MATCH_2}")

# This is just for the clangd LSP
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Global options
option(NORNG "Uses the same set seed. Useful for testing." OFF)
option(OPENMP "Enable/Disable OpenMP." OFF)
option(LAPACK "Enable/Disable LAPACK." OFF)
option(LTRACE "Enable trace logging." OFF)
option(PPROF "Enable performance counter profilling." OFF)

if(LTRACE)
    add_definitions(-DLTRACE)
endif()

#####################################
#####################################

set(project_name nested_fit)
project(${project_name} VERSION ${project_version} LANGUAGES Fortran CXX)

# Enforce C++17
set(CMAKE_CXX_STANDARD 17)

# Resman producer
include(cmake/resman.cmake)

# Manifest writing tool
include(cmake/write_manifest.cmake)

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

# Set bin output dir
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# Set nested fit version strings
set(nested_fit_version_full_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}${dev_version}")
set(nested_fit_version_str "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}")
string(CONCAT nested_fit_target "nested_fit" ${nested_fit_version_full_str})
string(REPLACE "." "_" nested_fit_target_ac ${nested_fit_target})

set(SRC_FILES_COMM
    src/Mod_cluster_analysis.f90
    src/Mod_parameters.f90
    src/Mod_covariance_matrix.f90
    src/Mod_search_new_point.f90    #
    src/Mod_timestamp.f90
    src/Mod_likelihood_gen.f90
    src/Mod_likelihood.f90   
    src/Mod_potentials.f90
    src/Mod_integrated_func.f90
    src/Mod_array_tries.f90

    src/USERFCN_2D.f90
    src/USERFCN_SET.f
    src/USERFCN.f

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

set(SRC_EXTERNAL_FUNCS
    src/helper/hash.f90
    src/Mod_interpolate.f90
    src/functions/internal_funcs.f90
    src/Mod_logger.f90      # NOTE :(César) Required for logging from the shared lib
    src/Mod_timestamp.f90   # NOTE :(César) Required for logging from the shared lib
    src/functions/WOFZ.f
)

file(GLOB SRC_DIERCKX RELATIVE ${CMAKE_SOURCE_DIR} src/DIERCKX/*.f)
file(GLOB SRC_SLATEC  RELATIVE ${CMAKE_SOURCE_DIR} src/SLATEC/*.f)

if(OPENMP)
    find_package(OpenMP)
    if(NOT OpenMP_FOUND)
        message(WARNING "OpenMP not supported but specified.")
        message(WARNING "Disabling OpenMP.")
        set(OPENMP OFF CACHE BOOL "" FORCE)
    endif()
endif()

if(OPENMP AND NORNG)
    message(ERROR "Having a set seed while enabling OpenMP does not work.")
    message(WARNING "Disabling OpenMP.")
    message(STATUS "Please enable only one feature.")
    set(OPENMP OFF CACHE BOOL "" FORCE)
endif()

if(OPENMP)
    set(nested_fit_parallel_on ".TRUE.")
else()
    set(nested_fit_parallel_on ".FALSE.")
endif()

if(NORNG)
    set(nested_fit_static_seed ".TRUE.")
else()
    set(nested_fit_static_seed ".FALSE.")
endif()

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
    src/helper/utils.cpp
    src/funceval/native_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/latex_parser.cpp
)
set(SRC_FILES_CXX_AC
    "src/helper/console.cpp src/helper/utils.cpp src/funceval/latex_parser.cpp src/funceval/native_parser.cpp"
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

set(SRC_EXTERNAL_ALL ${SRC_EXTERNAL_FUNCS} ${SRC_DIERCKX})
add_library(internal_functions OBJECT ${SRC_EXTERNAL_ALL})

set(OBJ_RES_FILES "")
foreach(src_file IN LISTS SRC_EXTERNAL_ALL)
	get_filename_component(src_dir ${src_file} DIRECTORY)
	get_filename_component(base_ext ${src_file} NAME)
	list(APPEND OBJ_RES_FILES ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/internal_functions.dir/${src_dir}/${base_ext}.o)
endforeach()

add_executable(${nested_fit_target}
	# Source files
    ${CFG_FILES}
    ${SRC_DIERCKX}
    ${SRC_SLATEC}
    ${SRC_FILES_COMM}
    ${SRC_FILES_CXX}
)

# NOTE: (César)
# Use resource manager to link to the built object files
# They get aggregated into .c files via xxd and compiled
# into object files. In turn these get linked to the bin.
# This creates C functions that are of the format:
# 'CopyCacheData<NAME>(const char* dir)'
# These funcitons copy the specified 'FILES' into:
# 'dir'/<OUTDIR>
rm_link_resources(
	NAME "ObjFiles"
	TARGET ${nested_fit_target}
	FILES ${OBJ_RES_FILES}
	DEPENDS internal_functions
	OUTDIR "sys"
)

rm_link_resources(
	NAME "ManifestFile"
	TARGET ${nested_fit_target}
	FILES ${CMAKE_BINARY_DIR}/manifest.txt
)

# TODO(César): This assumes we are using gfortran!
set(SRC_FLAGS_D     -cpp -O0 -Wall -g -ffree-line-length-0 -ffixed-line-length-0 -Wno-ampersand -Wno-tabs -Wno-unused-dummy-argument)
set(SRC_FLAGS_R     -cpp -O2 -static -ffree-line-length-0 -ffixed-line-length-0)

set(SRC_FLAGS_CXX_D -O0 -Wall -pedantic -g -Wno-missing-field-initializers)      # Be pedantic
set(SRC_FLAGS_CXX_R -O2 -Wall -pedantic -static -Wno-missing-field-initializers) # Be pedantic on release too

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

if(LAPACK)
    target_compile_definitions(${nested_fit_target}      PUBLIC -DLAPACK_ON)
    if(UNIX)
        # Find LAPACK and BLAS on Unix-like systems
        # old try set(SRC_FLAGS "${SRC_FLAGS} -L/usr/lib/x86_64-linux-gnu/liblapack.so -llapack -L/usr/lib/x86_64-linux-gnu/libblas.so -lblas")
        find_package(LAPACK REQUIRED)  
        if(LAPACK_FOUND)
            target_link_libraries(${nested_fit_target} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
        else()
            message(FATAL_ERROR "LAPACK library not found. Please install LAPACK.")
        endif()
    elseif(APPLE)
        find_library(ACCELERATE_FRAMEWORK Accelerate)
        target_link_libraries(${nested_fit_target}      PRIVATE ${ACCELERATE_FRAMEWORK})
    elseif(WIN32)
        # Specify LAPACK and BLAS paths and names on Windows
        #set(LAPACK_LIBRARIES "path/to/lapack.lib")
        #set(BLAS_LIBRARIES "path/to/blas.lib")
        #target_link_libraries(${nested_fit_target} PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
        message(FATAL_ERROR "LAPACK library not found. Please install LAPACK.")
    endif()
endif()

target_link_libraries(${nested_fit_target} PUBLIC dl)
target_link_libraries(${nested_fit_target} PRIVATE TracyClient)
target_include_directories(${nested_fit_target} PRIVATE ${tracy_SOURCE_DIR})


if(OPENMP)
    target_link_options(${nested_fit_target} PRIVATE "-fopenmp")
endif()

if(NORNG)
    # Set the NORNG_ON flag for the preprocessor
    target_compile_definitions(${nested_fit_target} PUBLIC -DNORNG_ON)
endif()

install(TARGETS ${nested_fit_target} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)

# Write a manifest file into the cache
# This makes things easier on the python side
# For now all the generated data can be generated
# on configure time
write_manifest()

include(CTest)

if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
	add_subdirectory(test)
endif()

enable_testing()
