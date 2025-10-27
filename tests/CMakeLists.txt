# This is the cmake build file for the tests directory of the UPP project.
#
# Alyson Stahl 10/2025

message(STATUS "Building test directory...")

# Run each shell test.
function(shell_test name)
  # Copy the test scripts.
  file(COPY "${CMAKE_SOURCE_DIR}/ci/jobs-dev/${name}.sh"
    DESTINATION ${CMAKE_BINARY_DIR}/tests
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
  # Add the shell script as a test.
  add_test(NAME ${name}.sh COMMAND bash ${name}.sh)
endfunction()

shell_test(run_post_gfs_templates)