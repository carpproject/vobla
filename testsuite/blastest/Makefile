# Copyright (c) 2013-2014, ARM Limited
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE

# Makefile to verify BLAS level 1, 2 & 3.
# Employs the BLAS test suites from http://www.netlib.org/blas/.

FC=gfortran
CC=gcc
VC=../../vobla
VOO=${PENCIL_HOME}/optimizer

FFLAGS=-m32 -ff2c -g
CFLAGS=-Wall -Wno-unknown-pragmas -m32 -std=c99 -g -I testfiles -include "testfiles/builtin.h"

# Functions to test in vobla
BLAS1OBJS=rotg.o rot.o swap.o scal.o copy.o axpy.o dot.o nrm2.o asum.o amax.o
BLAS2OBJS=gemv.o trmv.o trsv.o ger.o her.o her2.o syr.o syr2.o
BLAS3OBJS=gemm.o syrk.o syr2k.o trmm.o trsm.o herk.o her2k.o

TESTBLAS1OBJS=$(addprefix blas1/,$(BLAS1OBJS)) testfiles/common.o blas1/wrapper.o blas1/pwrapper.o
TESTBLAS2OBJS=$(addprefix blas2/,$(BLAS2OBJS)) testfiles/lsame.o testfiles/common.o blas2/wrapper.o blas2/pwrapper.o
TESTBLAS3OBJS=$(addprefix blas3/,$(BLAS3OBJS)) testfiles/lsame.o testfiles/common.o blas3/wrapper.o blas3/pwrapper.o

all: runtests

blas1/wrapper.c blas1/pwrapper.c: wrapper

blas2/wrapper.c blas2/pwrapper.c: wrapper

blas3/wrapper.c blas3/pwrapper.c: wrapper

testfiles/testsblas1: $(TESTBLAS1OBJS) testfiles/sblat1.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testsblas2: $(TESTBLAS2OBJS) testfiles/sblat2.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testsblas3: $(TESTBLAS3OBJS) testfiles/sblat3.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testdblas1: $(TESTBLAS1OBJS) testfiles/dblat1.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testdblas2: $(TESTBLAS2OBJS) testfiles/dblat2.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testdblas3: $(TESTBLAS3OBJS) testfiles/dblat3.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testcblas1: $(TESTBLAS1OBJS) testfiles/cblat1.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testcblas2: $(TESTBLAS2OBJS) testfiles/cblat2.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testcblas3: $(TESTBLAS3OBJS) testfiles/cblat3.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testzblas1: $(TESTBLAS1OBJS) testfiles/zblat1.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testzblas2: $(TESTBLAS2OBJS) testfiles/zblat2.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

testfiles/testzblas3: $(TESTBLAS3OBJS) testfiles/zblat3.o
	$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@

%.c.base : %.vobla
	$(VC) $^ -o $@

%.c : %.c.base
	$(VOO) $^ -o $@

wrapper: testfiles/wrapper.py
	python testfiles/wrapper.py --all

runblas1: testfiles/testdblas1 testfiles/testsblas1 testfiles/testcblas1 testfiles/testzblas1 outputdir
	./testfiles/testsblas1 > output/SBLAT1.SUMM
	./testfiles/testdblas1 > output/DBLAT1.SUMM
	./testfiles/testcblas1 > output/CBLAT1.SUMM
	./testfiles/testzblas1 > output/ZBLAT1.SUMM

runblas2: testfiles/testdblas2 testfiles/testsblas2 testfiles/testcblas2 testfiles/testzblas2 outputdir
	./testfiles/testsblas2 < testfiles/sblat2.dat
	./testfiles/testdblas2 < testfiles/dblat2.dat
	./testfiles/testcblas2 < testfiles/cblat2.dat
	./testfiles/testzblas2 < testfiles/zblat2.dat

runblas3: testfiles/testdblas3 testfiles/testsblas3 testfiles/testcblas3 testfiles/testzblas3 outputdir
	./testfiles/testsblas3 < testfiles/sblat3.dat
	./testfiles/testdblas3 < testfiles/dblat3.dat
	./testfiles/testcblas3 < testfiles/cblat3.dat
	./testfiles/testzblas3 < testfiles/zblat3.dat

outputdir:
	mkdir -p output

runtests: cleanoutput runblas1 runblas2 runblas3
	./summary.sh

clean: cleanoutput
	rm -f *.o
	rm -f blas1/*.o
	rm -f blas1/*.c
	rm -f blas2/*.o
	rm -f blas2/*.c
	rm -f blas3/*.o
	rm -f blas3/*.c
	rm -f blas*/*.c.base
	rm -f testfiles/test?blas?
	rm -f sparse/exports.c
	rm -f sparse/exports.c.base
	rm -f testfiles/*.o

cleanoutput:
	rm -f output/*

blastestfiles:
	./testfiles/fetch.sh

.PHONY: clean cleanoutput runtests runblas1 runblas2 runblas3
.SECONDARY:
