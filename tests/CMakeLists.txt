# Function to add a test in Fortran that adheres to a simple naming
# convention.
function(add_simple_Fortran_test test_name)

  add_executable(${test_name} ${test_name}.F90)

  # Link to the library being tested.
  target_link_libraries(${test_name} nobarrier_omp)

  add_test(NAME ${test_name} COMMAND env OMP_NUM_THREADS=3 ${test_name})

endfunction()

add_simple_Fortran_test(test_barrier)
add_simple_Fortran_test(test_noblock)
add_simple_Fortran_test(test_reuse)
add_simple_Fortran_test(test_half_barrier)
