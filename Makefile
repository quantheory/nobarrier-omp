FC ?= gfortran-4.7.3
FFLAGS += -fopenmp

all: test_barrier test_noblock test_reuse

test: test_barrier test_noblock test_reuse
	test_barrier
	test_noblock
	test_reuse

.PHONY: all test clean

TBAR_OBJS := test_barrier.o noblock_omp.o

test_barrier: $(TBAR_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TNB_OBJS := test_noblock.o noblock_omp.o

test_noblock: $(TNB_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TRU_OBJS := test_reuse.o noblock_omp.o

test_reuse: $(TRU_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_barrier.o: noblock_omp.mod
test_noblock.o: noblock_omp.mod
test_reuse.o: noblock_omp.mod

noblock_omp.mod: noblock_omp.o
	-@:

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	$(RM) *.o *.mod test_barrier test_noblock test_reuse
