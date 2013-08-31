# Function to add a test in Fortran that adheres to a simple naming
# convention.
function(add_simple_Fortran_test test_name)

  add_executable(${test_name} ${test_name}.F90)

  # Link to the library being tested.
  target_link_libraries(${test_name} nobarrier_omp)

  add_test(NAME ${test_name}
    COMMAND env OMP_NUM_THREADS=2 ${test_name})

  set_tests_properties(${test_name}
    PROPERTIES FAIL_REGULAR_EXPRESSION failed)

  add_executable(${test_name}_serial ${test_name}.F90)

  # Link to the library being tested.
  target_link_libraries(${test_name}_serial nobarrier_omp)

  add_test(NAME ${test_name}_serial
    COMMAND env OMP_NUM_THREADS=1 ${test_name}_serial)

endfunction()

add_simple_Fortran_test(test_barrier)
add_simple_Fortran_test(test_noblock)
add_simple_Fortran_test(test_reuse)
add_simple_Fortran_test(test_half_barrier)
