# Brief  : Create a manifest.txt file in the cache after installing
# Author : César Godinho
# Date   : 15/07/24

function(write_manifest)
    # Clear file
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "")

    # Selectively choose what to write to the manifest file
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "[Features]\n")
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "OPENMP=${OPENMP}\n")
    if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "BUILDTYPE=Debug\n")
    else()
        file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "BUILDTYPE=Release\n")
    endif()
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "PPROF=${PPROF}\n")
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/manifest.txt "LTRACE=${LTRACE}\n")
endfunction()
