# This is the cmake build file for the tests directory of the UPP project.
#
# Alyson Stahl 10/2025

message(STATUS "Building test directory...")

# This function builds, links, and runs a test program.
function(create_test name)
  add_executable(${name} ${name}.f)
  target_link_libraries(${name} PRIVATE upp::upp)
  add_test(NAME ${name} COMMAND ${name})
endfunction()

create_test(unit_test)