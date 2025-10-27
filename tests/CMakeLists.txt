# This is the cmake build file for the tests directory of the UPP project.
#
# Alyson Stahl 10/2025

message(STATUS "Building test directory...")

# Run each regression test.
function(regression_test name)
  # Copy the test scripts.
  file(COPY "${CMAKE_SOURCE_DIR}/ci/jobs-dev/run_post_${name}_template.sh"
    DESTINATION ${CMAKE_BINARY_DIR}/tests
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
  # Add the shell script as a test.
  add_test(NAME run_post_${name}_template.sh COMMAND bash run_post_${name}_template.sh)
endfunction()


regression_test(gfs)