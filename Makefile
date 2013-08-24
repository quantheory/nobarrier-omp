FC ?= gfortran-4.7.3
FFLAGS += -fopenmp

all: test_noblock_barrier

test: test_noblock_barrier
	test_noblock_barrier

.PHONY: all test clean

OBJS := test_noblock_barrier.o noblock_barrier.o

test_noblock_barrier: $(OBJS)
	$(FC) $(FFLAGS) $^ -o $@

test_noblock_barrier.o: noblock_barrier.mod
noblock_barrier.mod: noblock_barrier.o
	-@:

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	$(RM) *.o *.mod test_noblock_barrier
