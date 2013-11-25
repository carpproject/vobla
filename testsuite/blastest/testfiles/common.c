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

#include "common.h"

int getTrans(char c) {
  switch(c) {
  case 'n': return NOTRANS;
  case 'N': return NOTRANS;
  case 't': return TRANS;
  case 'T': return TRANS;
  case 'c': return CONJTRANS;
  case 'C': return CONJTRANS;
  }

  return NOTRANS;
}

MatrixView GenMatrixView(int* m, int* n, int* ld, int transposed) {
  MatrixView view;

  view.raw.base_size2 = *ld;
  view.offset1 = 0;
  view.offset2 = 0;
  if((transposed & TRANS) == 1) {
    view.raw.base_size1 = *m;
    view.view_size1 = *m;
    view.view_size2 = *n;
  }
  else {
    view.raw.base_size1 = *n;
    view.view_size1 = *n;
    view.view_size2 = *m;
  }

  return view;
}

VectorView GenVectorView(int* size, int* increment) {
  VectorView view;
  view.raw.base_size1 = *size;
  view.offset1 = *increment > 0 ? 0 : (*size - 1);
  view.raw.base_size2 = abs(*increment);
  view.offset2 = 0;
  view.view_size = *size;
  return view;
}

int transpose(int x) { return x ^ TRANS; }
int getSide(char c) { return c == 'l' || c == 'L' ? LEFT : RIGHT; }
int getUplo(char c) { return c == 'l' || c == 'L' ? LOWER : UPPER; }
int getDiag(char c) { return c == 'u' || c == 'U' ? UNIT : NONUNIT; }
