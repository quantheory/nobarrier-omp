# Add a nobarrier_omp test following a simple convention.
function(add_nobarrier_omp_test test_name)

  add_executable(${test_name} ${test_name}.F90 utils.F90)

  target_link_libraries(${test_name} nobarrier_omp)

  add_test(NAME ${test_name}
    COMMAND env OMP_NUM_THREADS=3 ${test_name})

  set_tests_properties(${test_name}
    PROPERTIES FAIL_REGULAR_EXPRESSION failed)

endfunction()

# These tests are for the test utilities themselves.
add_nobarrier_omp_test(test_status_pass)

add_nobarrier_omp_test(test_status_fail)
set_tests_properties(test_status_fail
  PROPERTIES WILL_FAIL TRUE)

add_nobarrier_omp_test(test_counter)

# Test executable.
set(pf_files test_soft_barrier.pf test_half_barrier.pf)
add_pFUnit_executable(pFUnit_test_exe "${pf_files}"
  ${CMAKE_CURRENT_BINARY_DIR} "utils.F90")

target_link_libraries(pFUnit_test_exe nobarrier_omp)

add_test(NAME pFUnit_test
  COMMAND env OMP_NUM_THREADS=3 pFUnit_test_exe)

define_pFUnit_failure(pFUnit_test)

# Half barrier tests.
add_nobarrier_omp_test(test_half_reinit)
