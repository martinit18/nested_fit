# Brief: Configuration of multiple tests
#        Coverage is low, but is good to start somewhere
# Author: César Godinho
# Date: 09/05/2024

message(STATUS "Configuring unit tests.")


add_executable(strsplit strsplit.f90 ../../src/helper/strutil.f90)
add_test(NAME strsplit COMMAND $<TARGET_FILE:strsplit>)
