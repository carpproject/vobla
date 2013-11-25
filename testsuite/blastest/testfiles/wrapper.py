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

import sys
from optparse import OptionParser

functions = (
#   name    type    return  lvl args
  [['asum',  'sdcz', 'real', 1, ['n', 'X']],
   ['axpy',  'sdcz', 'void', 1, ['n', 'alpha', 'X', 'Y']],
   ['copy',  'sdcz', 'void', 1, ['n', 'X', 'Y']],
   ['dot',   'sd',   'real', 1, ['n', 'X', 'Y']],
   ['dotc',  'cz',   'void', 1, ['out', 'n', 'X', 'Y']],
   ['dotu',  'cz',   'void', 1, ['out', 'n', 'X', 'Y']],
   ['nrm2',  'sdcz', 'real', 1, ['n', 'X']],
   ['rot',   'sd',   'void', 1, ['n', 'X', 'Y', 'c', 's']],
   ['scal',  'sdcz', 'void', 1, ['n', 'alpha', 'X']],
   ['sscal', 'c',    'void', 1, ['n', 'alpha', 'X']],
   ['dscal', 'z',    'void', 1, ['n', 'alpha', 'X']],
   ['swap',  'sdcz', 'void', 1, ['n', 'X', 'Y']],
   ['amax',  'sdzc', 'int',  1, ['n', 'X']],
   ['rotg',  'sd',   'void', 1, []],
#  name     type    return  lvl args
   ['gemv', 'sdcz', 'void', 2.1, ['tA', 'm', 'n', 'alpha', 'A', 'X', 'beta', 'Y']],
   ['gbmv', 'sdcz', 'void', 2.1, ['tAB', 'm', 'n', 'kl', 'ku', 'alpha', 'AB', 'X', 'beta', 'Y']],
   ['hemv', 'cz',   'void', 2.1, ['uplo', 'n', 'alpha', 'AT', 'X', 'beta', 'Y']],
   ['hbmv', 'cz',   'void', 2.1, ['uplo', 'n', 'k', 'alpha', 'ABT', 'X', 'beta', 'Y']],
   ['hpmv', 'cz',   'void', 2.1, ['uplo', 'n', 'alpha', 'AP', 'X', 'beta', 'Y']],
   ['symv', 'sd',   'void', 2.1, ['uplo', 'n', 'alpha', 'AT', 'X', 'beta', 'Y']],
   ['sbmv', 'sd',   'void', 2.1, ['uplo', 'n', 'k', 'alpha', 'ABT', 'X', 'beta', 'Y']],
   ['spmv', 'sd',   'void', 2.1, ['uplo', 'n', 'alpha', 'AP', 'X', 'beta', 'Y']],
   ['trmv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'AT', 'X', 'triangular']],
   ['tbmv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'k', 'ABT', 'X', 'triangular']],
   ['tpmv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'AP', 'X', 'triangular']],
   ['trsv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'AT', 'X', 'triangular']],
   ['tbsv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'k', 'ABT', 'X', 'triangular']],
   ['tpsv', 'sdcz', 'void', 2.1, ['uplo', 'tA', 'diag', 'n', 'AP', 'X', 'triangular']],
#  name     type    return  lvl args
   ['ger',  'sd',   'void', 2.2, ['m', 'n', 'alpha', 'X', 'Y', 'A']],
   ['geru', 'cz',   'void', 2.2, ['m', 'n', 'alpha', 'X', 'Y', 'A']],
   ['gerc', 'cz',   'void', 2.2, ['m', 'n', 'alpha', 'X', 'Y', 'A']],
   ['her',  'cz',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'AT', 'fillDiagIm']],
   ['hpr',  'cz',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'AP', 'fillDiagIm']],
   ['her2', 'cz',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'Y', 'AT', 'fillDiagIm']],
   ['hpr2', 'cz',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'Y', 'AP', 'fillDiagIm']],
   ['syr',  'sd',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'AT']],
   ['spr',  'sd',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'AP']],
   ['syr2', 'sd',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'Y', 'AT']],
   ['spr2', 'sd',   'void', 2.2, ['uplo', 'n', 'alpha', 'X', 'Y', 'AP']],
#  name     type    return  lvl args
   ['gemm', 'sdcz', 'void', 3, ['tA', 'tB', 'm', 'n', 'k', 'alpha', 'A', 'B', 'beta', 'C'],
      ['ttt', 'ntt', 'ctt', 'tnt', 'nnt', 'cnt', 'tct', 'nct', 'cct']],
   ['symm', 'sdcz', 'void', 3, ['side', 'uplo', 'm', 'n', 'alpha', 'AT', 'B', 'beta', 'C'],
      ['t', 'n']],
   ['hemm', 'cz'  , 'void', 3, ['side', 'uplo', 'm', 'n', 'alpha', 'AT', 'B', 'beta', 'C'],
      ['t', 'n']],
   ['syrk', 'sdcz', 'void', 3, ['uplo', 'tA', 'n', 'k', 'alpha', 'A', 'beta', 'CT'],
      ['t', 'n']],
   ['herk', 'cz'  , 'void', 3, ['uplo', 'tA', 'n', 'k', 'alpha', 'A', 'beta', 'CT', 'fillDiagIm'],
      ['t', 'c']],
   ['syr2k','sdcz', 'void', 3, ['uplo', 'tA', 'n', 'k', 'alpha', 'A', 'B', 'beta', 'CT'],
      ['tt', 'nn']],
   ['her2k','cz'  , 'void', 3, ['uplo', 'tA', 'n', 'k', 'alpha', 'A', 'B', 'beta', 'CT', 'fillDiagIm'],
      ['tt', 'cc']],
   ['trmm', 'sdcz', 'void', 3, ['side', 'uplo', 'tA', 'diag', 'm', 'n', 'alpha', 'A', 'B'],
      ['tt', 'nt', 'ct', 'tn', 'nn', 'un']],
   ['trsm', 'sdcz', 'void', 3, ['side', 'uplo', 'tA', 'diag', 'm', 'n', 'alpha', 'A', 'B'],
      ['tt', 'nt', 'ct', 'tn', 'nn', 'un']],
   ])


def findFunction(name):
  for f in functions:
    if f[0] == name:
      return f
  print "Unknown function " + name + "\n"
  sys.exit(1)

def intersect(a, b):
  return list(set(a) & set(b))

def join(ins):
  out = ['']
  for options in ins:
    newOut = []
    for opt in options:
      for v in out:
        newOut.append(v + opt)
    out = newOut
  return out

def generateIncludes():
  return '#include "common.h"\n'

class WrapperType:
  def __init__(self, name, typeId, returnType):
    # generating value type
    if typeId == 's':
      self.valueType = 'float'
    elif typeId == 'd':
      self.valueType = 'double'
    elif typeId == 'c':
      self.valueType = 'ComplexFloat'
    elif typeId == 'z':
      self.valueType = 'ComplexDouble'
    else:
      raise Exception('bad type :' + typeId)
    # generating members
    self.typeId = typeId
    self.isComplex = typeId in 'cz'
    self.realType = 'float' if typeId in 'sc' else 'double'
    self.returnType = self.realType if returnType == 'real' else returnType
    self.name = typeId + name
    # Generating retuen type
    if returnType == 'int':
      self.name = 'i' + self.name
    elif returnType == 'real' and self.isComplex:
      self.name = ('s' if typeId in 'sc' else 'd') + self.name

def processWrapperArg(name, t, isVoblaArg):
  wArgs.append(t + '* ' + name)
  if isVoblaArg:
    fArgs.append('*' + name)
    dArgs.append(t)

def processArrayArg(name, dim, vType, arg2, argList):
  global body
  wArgs.append(vType + '* ' + name + ', int* ' + arg2 + name)
  viewName = name + '_view'
  if options.flatten:
    for i in range(5):
      dArgs.append('int')
    fArgs.append(viewName + '.raw.base_size1')
    fArgs.append(viewName + '.raw.base_size2')
    if dim == 'Vector':
      fArgs.append(viewName + '.view_size')
    else:
      fArgs.append(viewName + '.view_size1')
      fArgs.append(viewName + '.view_size2')
      dArgs.append('int')
    fArgs.append(viewName + '.offset1')
    fArgs.append(viewName + '.offset2')
  else:
    fArgs.append(viewName)
    dArgs.append(dim + 'View')
  fArgs.append(name)
  dArgs.append(vType + '*')
  body += '  ' + dim + 'View ' + viewName + ' = '
  body += 'Gen' + dim + 'View(' + ', '.join(argList) + ');\n'

def processFlagArg(name, flagType, isAnArg):
  global body
  wArgs.append('char* ' + name + '_')
  body += '  int ' + name + '= get' + flagType + '(*' + name + '_);\n'
  if isAnArg:
    fArgs.append(name)
    dArgs.append('int')

def processRotgArg(wType):
  global body
  body += '  int one = 1;\n'
  body += '  VectorView one_view = GenVectorView(&one, &one);\n'
  for name in 'abcs':
    wArgs.append(wType.valueType + '* ' + name)
    fArgs.append('one_view, ' + name)
    dArgs.append('VectorView, ' + wType.valueType + '*')

def initCall(name, returnType, typedName, args):
  call = '  '
  if 'out' in args:
    call += '*out = '
  elif returnType != 'void':
    call += 'return '
  if name == 'amax':
    call += '*n > 0 ? '
  call += typedName
  return call

def endCall(args, name):
    call = '(' + ', '.join(args) + ')'
    if name == 'amax':
      call += '+1 : 0'
    return call + ';\n'

def genCondTranspose(cond, toTrans):
  global body
  body += '  if(' + cond + ') {\n'
  for name in toTrans:
    if name == 'side':
      body += '    side = side == LEFT ? RIGHT : LEFT;\n'
    elif name == 'Ac':
      body += '    tA = tA ^ CONJ;\n'
    elif name == 'Ch':
      body += '    tC = tC ^ CONJTRANS;\n'
    elif name == 'Ah':
      body += '    tA = tA ^ CONJTRANS;\n'
    else:
      body += '    t' + name + ' = transpose(t' + name + ');\n'
  body += '  }\n'

def getTransSwitch(i, m, isComplex, transpose):
  name = 't' + m
  if not isComplex:
    name = '(' + name + ' & 1)'
  if not transpose:
    name = '(' + name + ' ^ 1)'
  return ' | (' + name + ' << ' + str(i) + ')'

def genSwitchValue(args, mc, isComplex):
  i = 0
  s = '0 '
  if not '' in mc:
    for m in ['A', 'B', 'C', 'AB']:
      if m in args:
        if not 'AT' in args or not m == 'B':
          s += getTransSwitch(i, m, isComplex, True)
          i += 2
  if 'triangular' in args:
    s += getTransSwitch(i, 'A', isComplex, 'AT' in args)
    s += ' | (diag << ' + str(i+2) + ')'
    i += 3
  if intersect(args, ['AP', 'ABT']):
    s += ' | (uplo << ' + str(i) + ')'
    i += 1
  elif intersect(['AT', 'CT'], args):
    s += ' | ((uplo ^ 1) << ' + str(i) + ')'
    i += 1
  for a in ['X', 'Y']:
    if a in args:
      s += ' | ((*inc' + a + ' > 0 ? 0 : 1) << ' + str(i) + ')'
      i += 1
  return s

def genSwitchCase(mCase, aCase, isComplex, callBeg, callEnd):
  i = 1;
  caseNumber = 0
  for n in mCase:
    if n == 'n':
      caseNumber += i
    elif n == 'c':
      caseNumber += i + 2*i
    elif n == 'u':
      caseNumber += 2*i
    i = 4*i
  for n in aCase:
    if n == 'r':
      caseNumber += i
    elif n == 'l':
      caseNumber += i
    elif n == 'd':
      caseNumber += i
    i = 2*i
  out = '  case ' + str(caseNumber) + ':\n  '
  out += callBeg  + getCaseSuffix(mCase, aCase) + callEnd
  return out + '    break;\n'

def processBandMatrixArg(wType):
  global body
  wArgs.append(wType.valueType + '* A, int* ldA')
  fArgs.append('A_view, A')
  dArgs.append('BandMatrixView, ' + wType.valueType + '*')
  body += '  BandMatrixView A_view;\n'
  body += '  A_view.storage.base_size1 = *n;\n'
  body += '  A_view.storage.base_size2 = *ldA;\n'
  body += '  A_view.m = *n;\n'
  body += '  A_view.n = *m;\n'
  body += '  A_view.kl = *ku;\n'
  body += '  A_view.ku = *kl;\n'

def processPackedMatrixArg(wType):
  global body
  wArgs.append(wType.valueType + '* AP')
  fArgs.append('AP_view, AP')
  dArgs.append('PackedTriangleView, ' + wType.valueType + '*')
  body += '  PackedTriangleView AP_view;\n'
  body += '  AP_view.storage.n = *n;\n'
  body += '  AP_view.n = *n;\n'
  body += '  AP_view.offset1 = 0;\n'
  body += '  AP_view.offset2 = 0;\n'

def processTriangularMatrixArg(wType, n):
  global body
  wArgs.append(wType.valueType + '* AT, int* ldA')
  fArgs.append('AT_view, AT')
  dArgs.append('TriangleView, ' + wType.valueType + '*')
  body += '  TriangleView AT_view;\n'
  body += '  AT_view.storage.base_size1 = *' + n + ';\n'
  body += '  AT_view.storage.base_size2 = *ldA;\n'
  body += '  AT_view.n = *' + n + ';\n'
  body += '  AT_view.offset1 = 0;\n'
  body += '  AT_view.offset2 = 0;\n'

def processTriangularMatrixArg(wType, n, name):
  global body
  wArgs.append(wType.valueType + '* ' + name + 'T, int* ld' + name)
  fArgs.append(name + 'T_view, ' + name + 'T')
  dArgs.append('TriangleView, ' + wType.valueType + '*')
  body += '  TriangleView ' + name + 'T_view;\n'
  body += '  ' + name + 'T_view.storage.base_size1 = *' + n + ';\n'
  body += '  ' + name + 'T_view.storage.base_size2 = *ld' + name + ';\n'
  body += '  ' + name + 'T_view.n = *' + n + ';\n'
  body += '  ' + name + 'T_view.offset1 = 0;\n'
  body += '  ' + name + 'T_view.offset2 = 0;\n'

def processTriangularBandMatrixArg(wType):
  global body
  wArgs.append(wType.valueType + '* ABT, int* ldA')
  fArgs.append('ABT_view, ABT')
  dArgs.append('BandTriangleView, ' + wType.valueType + '*')
  body += '  BandTriangleView ABT_view;\n'
  body += '  ABT_view.storage.base_size1 = *n;\n'
  body += '  ABT_view.storage.base_size2 = *ldA;\n'
  body += '  ABT_view.n = *n;\n'
  body += '  ABT_view.k = *k;\n'
  body += '  ABT_view.offset1 = 0;\n'
  body += '  ABT_view.offset2 = 0;\n'

def getCaseSuffix(mc, ac):
  suffix = ''
  if mc != '':
    suffix += '_' + mc
  if ac != '':
    suffix += '_' + ac
  return suffix

def genWrapper(name, wType, level, args, mCases):
  global wArgs, fArgs, dArgs, body
  fReturnType = wType.returnType
  wArgs = []
  fArgs = []
  dArgs = []
  body = ''
  # Return imediatly form the wrapper when BLAS does a quick return
  if 'out' in args:
    body += '  out->Re = 0;\n'
    body += '  out->Im = 0;\n'
    fReturnType = wType.valueType
    wArgs.append(fReturnType + '*' + ' out')
  if wType.returnType != 'void':
    body += '  ' + wType.returnType + ' out;\n'
    body += '  out = 0;\n'
  if 'n' in args:
    body += '  if(*n == 0) return'
    if wType.returnType != 'void':
      body += '  out'
    body += ';\n'
  if name == 'herk':
    body += '  if(*beta == 1.0 && (*alpha == 0.0 || *k == 0)) return;\n'
  if name == 'her2k':
    body += '  if(*beta == 1.0 && ((alpha->Re == 0.0 && alpha->Im == 0.0) || *k == 0)) return;\n'
  if name in ['her', 'hpr']:
    body += '  if(*alpha == 0.0) return;\n'
  if name in ['her2', 'hpr2']:
    body += '  if(alpha->Re == 0.0 && alpha->Im == 0.0) return;\n'
  # set all the flags
  n = 'n'
  m = 'm' if 'm' in args else 'n'
  k = 'k' if 'k' in args else 'm'
  if name == 'gemv':
    nRowA = 'tA == NOTRANS ? m : n'
    nColA = 'tA == NOTRANS ? n : m'
  else:
    nRowA = 'aSize' if 'side' in args else m
    nColA = 'aSize' if 'side' in args else n if int(level) == 2 else k
  if name in ['gemv', 'gbmv']:
    transName = 'tAB' if 'AB' in args else 'tA'
    sizeX = transName + ' == NOTRANS ? n : m'
    sizeY = transName + ' == NOTRANS ? m : n'
  else:
    sizeX = n if level == 2.1 else m
    sizeY = m if level == 2.1 else n
  # process all the arguments
  for a in args:
    if a in ['side']:
      processFlagArg(a, 'Side', name in ['trmm', 'trsm'])
      body += '  int* aSize = side == LEFT ? m : n;\n'
    elif a in ['uplo']:
      processFlagArg(a, 'Uplo', int(level) == 2 and not intersect(['AP', 'AT', 'ABT'], args))
    elif a in ['tA', 'tB', 'tC', 'tAB']:
      processFlagArg(a, 'Trans', False)
    elif a in ['diag']:
      processFlagArg(a, 'Diag', not 'triangular' in args)
    elif a in ['m', 'n', 'k', 'kl', 'ku']:
      processWrapperArg(a, 'int', False)
    # Process values
    elif a in ['alpha', 'beta', 'c', 's']:
      isReal = (a == 'alpha' and name in ['herk', 'sscal', 'dscal', 'her', 'hpr']) \
        or (a == 'beta' and name in ['herk', 'her2k'])
      processWrapperArg(a, wType.realType if isReal else wType.valueType, True)
    # Process arrays
    elif a in ['X', 'Y']:
      size = sizeX if a in ['X'] else sizeY
      processArrayArg(a, 'Vector', wType.valueType, 'inc', [size, 'inc' + a])
    # Process matrices
    elif a in ['A', 'B', 'C']:
      nRow = k if a == 'B' else nRowA if a == 'A' else m
      nCol = nColA if a == 'A' else n
      if 't' + a not in args:
        defaultTrans = 'tA' if a in ['B'] and name in ['syr2k', 'her2k'] else 'NOTRANS'
        body += '  int t' + a + ' = ' + defaultTrans + ';\n'
      processArrayArg(a, 'Matrix', wType.valueType, 'ld', [nRow, nCol, 'ld' + a, 't' + a])
    elif a in ['AB']:
      processBandMatrixArg(wType)
    elif a in['AP']:
      processPackedMatrixArg(wType)
    elif a in['AT']:
      processTriangularMatrixArg(wType, nRowA, 'A')
    elif a in['CT']:
      processTriangularMatrixArg(wType, n, 'C')
    elif a in['ABT']:
      processTriangularBandMatrixArg(wType)
  # rotg if totaly different from the rest
  if name == 'rotg':
    processRotgArg(wType)
  # generate transpositions
  if name in ['trmm', 'trsm']:
    body += '  uplo ^= (tA & TRANS);\n'
  if name in ['trmm', 'trsm']:
    genCondTranspose('uplo == LOWER', intersect(['A', 'B', 'side'], args))
  elif intersect([name], ['symm', 'hemm']) :
    genCondTranspose('side == RIGHT', ['C'])
  # Handle hermitian matrices
  if 'fillDiagIm' in args:
    body += '  for(int i=0; i<*n; i++) {\n'
    for name in 'AC':
      if name + 'T' in args:
        body += '    ' + name + 'T[i*(1 + *ld' + name + ')].Im = 0;\n'
    if 'AP' in args:
      body += '    if(uplo)\n'
      body += '      AP[(i*(2*(*n)-i+1))/2].Im = 0;\n'
      body += '    else\n'
      body += '      AP[(i*(i+3))/2].Im = 0;\n'
    body += '  }\n'
    # Generate the wrapper
  aCasesOpt = []
  if 'triangular' in args:
    aCasesOpt.append('dn')
  if intersect(['AP', 'AT', 'ABT', 'CT'], args):
    aCasesOpt.append('ul')
  for x in intersect(['X', 'Y'], args):
    aCasesOpt.append('nr')
  aCases = join(aCasesOpt)
  decl = ''
  callBeg = initCall(name, wType.returnType, wType.name, args)
  callEnd = endCall(fArgs, name)
  body += '  switch(' + genSwitchValue(args, mCases, wType.isComplex) + ') {\n'
  for mc in mCases:
    if (not wType.isComplex) and ('c' in mc or 'u' in mc):
      continue
    for ac in aCases:
      decl += fReturnType + ' ' + wType.name + getCaseSuffix(mc, ac)
      decl += '(' + ', '.join(dArgs) + ');\n'
      body += genSwitchCase(mc, ac, wType.isComplex, callBeg, callEnd)
  body += '  }\n'
  wrapper = decl + wType.returnType + ' ' + wType.name + '_(' + ', '.join(wArgs) + ') {\n'
  wrapper += body
  if wType.returnType == 'void':
    wrapper += '  return;\n'
  else:
    wrapper += '  return out;\n'
  wrapper += '}\n'
  return wrapper

def genWrappersForFunction(f):
  """Generate all wrappers for a BLAS function, e.g. saxpy, daxpy, caxpy, ..."""
  if f[3] == 3:
    mCases = f[5]
  elif f[0] in ['gemv', 'trmv', 'trsv', 'gbmv']:
    mCases = ['t', 'n', 'c']
  elif f[0] in ['tpmv', 'tpsv', 'tbmv', 'tbsv']:
    mCases = ['t', 'n', 'u']
  else:
    mCases = ['']
  # generate different versions
  wrapper = ""
  for t in f[1]:
    wrapper += genWrapper(f[0], WrapperType(f[0], t, f[2]), f[3], f[4], mCases)
  return wrapper

def generateAll():
  """Generate wrappers for all BLAS functions."""
  files = dict()
  # generate the files
  for lvl in [1, 2, 3]:
    files[lvl] = open('blas' + str(lvl) + '/wrapper.c', 'w')
    files[lvl].write(generateIncludes())
  # generate the wrappers
  for f in functions:
    wrappers = genWrappersForFunction(f)
    files[int(f[3])].write(wrappers)

#############################
##### Argument handling #####
#############################

def argSetup():
  global options
  global args
  usage = "Usage: %prog [options] [list of BLAS functions]"
  parser = OptionParser(usage=usage, description='Generates C wrappers for VOBLA BLAS functions.')
  parser.add_option('--flatten', dest='flatten', action='store_true', help='flatten structs')
  parser.add_option('--all', dest='generate_all', action='store_true', help='generate wrappers for all BLAS functions')
  (options, args) = parser.parse_args()

#############################
#####   Main function   #####
#############################

def main():
  argSetup()
  if options.generate_all:
    generateAll()
  else:
    wrappers = generateIncludes()
    for f in args:
      wrappers += genWrappersForFunction(findFunction(f))
    print wrappers

if __name__ == "__main__":
  main()
