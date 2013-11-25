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

FILES="cblat1 cblat2 cblat3 dblat1 dblat2 dblat3 sblat1 sblat2 sblat3 zblat1 zblat2 zblat3"
DATA_FILES="cblat2 cblat3 dblat2 dblat3 sblat2 sblat3 zblat2 zblat3"

URL=http://www.netlib.org/blas/

LABEL=DOWNLOADED

script="`readlink -f ${BASH_SOURCE[0]}`"
scriptpath="`dirname $script`"

cd ${scriptpath}

if [ -e $LABEL ]
then
    exit
fi

for file in $FILES
do
    wget ${URL}${file} -O ${file}.f
done

for data in $DATA_FILES
do
    wget ${URL}${data}d -O ${data}.dat
done

wget ${URL}lsame.f -O lsame.f

patch -p4 < dat.patch

touch ${LABEL}
