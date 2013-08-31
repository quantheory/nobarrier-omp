FC ?= gfortran-4.7.3
FFLAGS += -fopenmp -fbounds-check

all: test_barrier test_noblock test_reuse test_half_barrier

test: test_barrier test_noblock test_reuse test_half_barrier
	test_barrier
	test_noblock
	test_reuse
	test_half_barrier

.PHONY: all test clean

TBAR_OBJS := test_barrier.o nobarrier_omp.o

test_barrier: $(TBAR_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TNB_OBJS := test_noblock.o nobarrier_omp.o

test_noblock: $(TNB_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TRU_OBJS := test_reuse.o nobarrier_omp.o

test_reuse: $(TRU_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

THB_OBJS := test_half_barrier.o nobarrier_omp.o

test_half_barrier: $(THB_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_barrier.o: nobarrier_omp.mod
test_noblock.o: nobarrier_omp.mod
test_reuse.o: nobarrier_omp.mod
test_half_barrier.o: nobarrier_omp.mod

nobarrier_omp.mod: nobarrier_omp.o
	-@:

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	$(RM) *.o *.mod test_barrier test_noblock test_reuse \
	test_half_barrier
