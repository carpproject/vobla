#!/bin/bash

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

# Prints a summary of the BLAS test suite output.


sblas1output=output/SBLAT1.SUMM
sblas2output=output/SBLAT2.SUMM
sblas3output=output/SBLAT3.SUMM

dblas1output=output/DBLAT1.SUMM
dblas2output=output/DBLAT2.SUMM
dblas3output=output/DBLAT3.SUMM

cblas1output=output/CBLAT1.SUMM
cblas2output=output/CBLAT2.SUMM
cblas3output=output/CBLAT3.SUMM

zblas1output=output/ZBLAT1.SUMM
zblas2output=output/ZBLAT2.SUMM
zblas3output=output/ZBLAT3.SUMM

echo ""
echo "-----------------------"
echo "        Summary"
echo "-----------------------"


sblas1fails=`grep -c "FAIL" ${sblas1output}`
sblas2fails=`grep -c "FAIL" ${sblas2output}`
sblas3fails=`grep -c "FAIL" ${sblas3output}`

dblas1fails=`grep -c "FAIL" ${dblas1output}`
dblas2fails=`grep -c "FAIL" ${dblas2output}`
dblas3fails=`grep -c "FAIL" ${dblas3output}`

cblas1fails=`grep -c "FAIL" ${cblas1output}`
cblas2fails=`grep -c "FAIL" ${cblas2output}`
cblas3fails=`grep -c "FAIL" ${cblas3output}`

zblas1fails=`grep -c "FAIL" ${zblas1output}`
zblas2fails=`grep -c "FAIL" ${zblas2output}`
zblas3fails=`grep -c "FAIL" ${zblas3output}`

echo ""
echo "SBLAS 1: $sblas1fails failure(s)"
echo "SBLAS 2: $sblas2fails failure(s)"
echo "SBLAS 3: $sblas3fails failure(s)"
echo ""
echo "DBLAS 1: $dblas1fails failure(s)"
echo "DBLAS 2: $dblas2fails failure(s)"
echo "DBLAS 3: $dblas3fails failure(s)"
echo ""
echo "CBLAS 1: $cblas1fails failure(s)"
echo "CBLAS 2: $cblas2fails failure(s)"
echo "CBLAS 3: $cblas3fails failure(s)"
echo ""
echo "ZBLAS 1: $zblas1fails failure(s)"
echo "ZBLAS 2: $zblas2fails failure(s)"
echo "ZBLAS 3: $zblas3fails failure(s)"
echo ""
