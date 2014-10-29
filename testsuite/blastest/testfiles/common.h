/*
 * Copyright (c) 2013-2014, ARM Limited
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "stdlib.h"
#define LEFT      0
#define RIGHT     1

#define UPPER     0
#define LOWER     1

#define NONUNIT   0
#define UNIT      1

#define NOTRANS   0
#define TRANS     1
#define CONJ      2
#define CONJTRANS 3

#ifdef SHOW_CALLED_PENCIL_FUNCTIONS
  #include <stdio.h>
  #define SHOW_PENCIL_FUNCTION(x) {fprintf(stdout, "PENCIL function: %s\n", (x));}
#else
  #define SHOW_PENCIL_FUNCTION(x)
#endif

typedef struct ComplexFloat {
  float Re;
  float Im;
} ComplexFloat;

typedef struct ComplexDouble {
  double Re;
  double Im;
} ComplexDouble;

struct ArrayView {
  int base_size1;
  int base_size2;
};

typedef struct VectorView {
  struct ArrayView raw;
  int view_size;
  int offset1;
  int offset2;
} VectorView;

typedef struct MatrixView {
  struct ArrayView raw;
  int view_size1;
  int view_size2;
  int offset1;
  int offset2;
} MatrixView;

typedef struct PackedTriangularStorage {
  int n;
} PackedTriangularStorage;

typedef struct PackedTriangleView {
  struct PackedTriangularStorage storage;
  int n;
  int offset1;
  int offset2;
} PackedTriangleView;

typedef struct TriangleView {
  struct ArrayView storage;
  int n;
  int offset1;
  int offset2;
} TriangleView;

typedef struct BandTriangleView {
  struct ArrayView storage;
  int n;
  int k;
  int offset1;
  int offset2;
} BandTriangleView;

typedef struct BandMatrixView {
  struct ArrayView storage;
  int m;
  int n;
  int kl;
  int ku;
} BandMatrixView;

int getTrans(char c);
int getSide(char c);
int getUplo(char c);
int getDiag(char c);
int transpose(int x);

MatrixView GenMatrixView(int* m, int* n, int* ld, int transposed);
VectorView GenVectorView(int* size, int* increment);
