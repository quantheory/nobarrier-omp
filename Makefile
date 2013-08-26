FC ?= gfortran-4.7.3
FFLAGS += -fopenmp

all: test_barrier test_noblock test_reuse

test: test_barrier test_noblock test_reuse
	test_barrier
	test_noblock
	test_reuse

.PHONY: all test clean

TBAR_OBJS := test_barrier.o noblock_barrier.o

test_barrier: $(TBAR_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TNB_OBJS := test_noblock.o noblock_barrier.o

test_noblock: $(TNB_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

TRU_OBJS := test_reuse.o noblock_barrier.o

test_reuse: $(TRU_OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_barrier.o: noblock_barrier.mod
test_noblock.o: noblock_barrier.mod
test_reuse.o: noblock_barrier.mod

noblock_barrier.mod: noblock_barrier.o
	-@:

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	$(RM) *.o *.mod test_barrier test_noblock test_reuse
