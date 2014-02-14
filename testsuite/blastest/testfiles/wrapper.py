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

# Generate wrappers for a BLAS function: a C wrapper and multiple specialized
# PENCIL wrappers.
#
# The C wrapper takes a FORTRAN-compatible argument list (in which all
# arguments are pointers) and calls the appropriate PENCIL wrapper.  For
# example, the C wrapper calls a "normal" or "reverse" PENCIL wrapper of a
# function taking a vector X depending on the sign of the incX argument.
#
# A PENCIL wrapper calls a VOBLA-generated PENCIL function, filling in as
# much arguments with constants as possible.  This way, the optimizer can drop
# most offset parameters (that are often 0 for BLAS), which simplifies
# code generation.

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
#  name     type    return  lvl args;  mCases
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

#############################
##### Common utilities  #####
#############################

def findFunction(name):
  for f in functions:
    if f[0] == name:
      return f
  print "Unknown function " + name + "\n"
  sys.exit(1)

def isConstArrayArg(funcname, argname):
  """Return True if the argument of the given function is const."""
  nonConstArrayArgs = {'A': ['rotg', 'her', 'her2', 'ger', 'syr', 'syr2'], \
                       'B': ['rotg', 'trsm', 'trmm'],
                       'C': ['rotg', 'syrk', 'syr2k', 'herk', 'her2k', 'gemm'], \
                       'X': ['rot', 'swap', 'scal', 'trmv', 'trsv'], \
                       'Y': ['rot', 'swap', 'copy', 'gemv', 'axpy']}
  if funcname in nonConstArrayArgs[argname]:
    return False
  else:
    return True

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

def getArrayCases(args):
  aCasesOpt = []
  if 'triangular' in args:
    # unit-triangular (d) or non-unit-triangular (n)
    aCasesOpt.append('dn')
  if intersect(['AP', 'AT', 'ABT', 'CT'], args):
    # upper (u) or lower (l) triangular
    aCasesOpt.append('ul')
  for x in intersect(['X', 'Y'], args):
    # normal (n) or reversed (r) vector access order
    aCasesOpt.append('nr')
  return join(aCasesOpt)

class WrapperType:
  def __init__(self, name, typeId, returnType):
    # Set value type
    if typeId == 's':
      self.valueType = 'float'
    elif typeId == 'd':
      self.valueType = 'double'
    elif typeId == 'c':
      self.valueType = 'struct ComplexFloat'
    elif typeId == 'z':
      self.valueType = 'struct ComplexDouble'
    else:
      raise Exception('bad type :' + typeId)
    self.typeId = typeId
    self.isComplex = typeId in 'cz'
    self.realType = 'float' if typeId in 'sc' else 'double'
    self.returnType = self.realType if returnType == 'real' else returnType
    self.name = typeId + name
    # Derive name from return type
    if returnType == 'int':
      self.name = 'i' + self.name
    elif returnType == 'real' and self.isComplex:
      self.name = ('s' if typeId in 'sc' else 'd') + self.name


#############################
#####    C Wrapper      #####
#############################

def gen2DArraySizeDeclFromView(arrayName):
  if options.flatten:
    return '[restrict const static ' + arrayName + '_view__storage__base_size0][' + arrayName + '_view__storage__base_size1]'
  else:
    return '[restrict const static ' + arrayName + '_view.storage.base_size0][' + arrayName + '_view.storage.base_size1]'

def gen2DArraySizeDecl(dim0, dim1):
  return '[restrict const static ' + dim0 + '][' + dim1 + ']'

def processWrapperArg(name, typ, isVoblaArg):
  cwrapArgs.append(typ + ' *' + name)
  pencilArgs.append(typ + ' ' + name)
  pencilCallArgs.append('*' + name)
  if isVoblaArg:
    if name in ['alpha', 'beta'] and 'Complex' in typ and options.flatten:
      primitiveType = 'float' if typ == 'struct ComplexFloat' else 'double'
      voblaArgs.append(primitiveType + ' ' + name + '__Re')
      voblaArgs.append(primitiveType + ' ' + name + '__Im')
      voblaCallArgs.append(name + '.Re')
      voblaCallArgs.append(name + '.Im')
    else:
      voblaArgs.append('const ' + typ + ' ' + name)
      voblaCallArgs.append(name)

def processArrayArg(name, dim, vType, arg2, arraySize, isConst):
  cwrapArgs.append(vType + ' *' + name)
  if arg2 != '':
    cwrapArgs.append('int *' + arg2 + name)
    pencilArgs.append('int ' + arg2 + name)
    pencilCallArgs.append('*' + arg2 + name)
  if not arraySize:
    pencilArgs.append(vType + ' ' + name + '[1]')
  elif len(arraySize) == 1 and dim == 'Vector':
    pencilArgs.append(vType + ' ' + name + '[restrict const static ' + arraySize[0] + '][1]')
  else:
    pencilArgs.append(vType + ' ' + name + '[restrict const static ' + ']['.join(arraySize) + ']')
  pencilCallArgs.append(name)
  viewName = 'one_view' if arraySize == '1' else name + '_view'
  # Generate arguments to VOBLA function
  if options.flatten:
    flatArgs = {'Vector':         ['storage__base_size0', 'storage__base_size1', 'view_size0', 'offset0', 'offset1'], \
                'Matrix':         ['storage__base_size0', 'storage__base_size1', 'view_size0', 'view_size1', 'offset0', 'offset1'], \
                'Triangle':       ['storage__base_size0', 'storage__base_size1', 'n', 'offset0', 'offset1'], \
                'BandMatrix':     ['storage__base_size0', 'storage__base_size1', 'm', 'n', 'kl', 'ku'], \
                'BandTriangle':   ['storage__base_size0', 'storage__base_size1', 'n', 'k', 'offset0', 'offset1'], \
                'PackedTriangle': ['storage__n', 'n', 'offset0', 'offset1']}
    for a in flatArgs[dim]:
      voblaArgs.append('int ' + name + "_view__" + a)

    # Storage
    if dim == 'PackedTriangle':
      voblaCallArgs.append(viewName + '.storage.n')
    else:
      voblaCallArgs.append(viewName + '.storage.base_size0')
      voblaCallArgs.append(viewName + '.storage.base_size1')

    # View, offset, etc.
    nonStorageStart = 1 if dim == 'PackedTriangle' else 2
    for a in flatArgs[dim][nonStorageStart:]:
      voblaCallArgs.append(viewName + '.' + a)

  else:
    voblaArgs.append('struct ' + dim + 'View ' + name + "_view")
    voblaCallArgs.append(viewName)
  optionalConst = 'const' if isConst == True else ''
  if dim == 'PackedTriangle':
    voblaArgs.append(optionalConst + ' ' + vType + ' ' + name + '[]')
  else:
    voblaArgs.append(optionalConst + ' ' + vType + ' ' + name + gen2DArraySizeDeclFromView(name))
  voblaCallArgs.append(name)

def processFlagArg(name, flagType, isVoblaArg, isPencilArg):
  body = ''
  cwrapArgs.append('char *' + name + '_')
  if isPencilArg:
    pencilArgs.append('int ' + name)
    pencilCallArgs.append(name)
  body += '  int ' + name + ' = get' + flagType + '(*' + name + '_);\n'
  if isVoblaArg:
    voblaArgs.append('int ' + name)
    voblaCallArgs.append(name)
  return body

def processRotgArg(wType):
  for name in 'abcs':
    processArrayArg(name, 'Vector', wType.valueType, '', '1', False)

def processBandMatrixArg(wType):
  processArrayArg('A', 'BandMatrix', wType.valueType, 'ld', ['1', '1'], True)

def processTriangularMatrixArg(wType, n, name):
  processArrayArg(name + 'T', 'Triangle', wType.valueType, 'ld', [n, n], True)

def processTriangularBandMatrixArg(wType):
  processArrayArg('ABT', 'BandTriangle', wType.valueType, 'ld', ['1', '1'], True)

def processPackedMatrixArg(wType):
  processArrayArg('AP', 'PackedTriangle', wType.valueType, '', [], True)

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
  body = ''
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
  return body

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

def getCaseSuffix(mc, ac):
  suffix = ''
  if mc != '':
    suffix += '_' + mc
  if ac != '':
    suffix += '_' + ac
  return suffix

def genWrapper(name, wType, level, args, mCases):
  global cwrapArgs, voblaArgs, pencilArgs, pencilCallArgs, voblaCallArgs
  fReturnType = wType.returnType
  cwrapArgs = []      # Arguments to C wrapper
  pencilArgs = []     # Arguments to PENCIL wrapper
  voblaArgs = []      # Arguments to VOBLA-generated PENCIL function
  pencilCallArgs = [] # Call arguments to PENCIL wrapper (called by the C wrapper)
  voblaCallArgs = []  # Call arguments to VOBLA function (called by the PENCIL wrapper)
  body = ''
  pencilBody = ''
  if 'out' in args:
    body += '  out->Re = 0;\n'
    body += '  out->Im = 0;\n'
    fReturnType = wType.valueType
    cwrapArgs.append(fReturnType + '*' + ' out')
  if wType.returnType != 'void':
    body += '  ' + wType.returnType + ' out;\n'
    body += '  out = 0;\n'
  # Return immediately from the wrapper when BLAS does a quick return
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
  nRowA = 'aSize' if 'side' in args else m
  nColA = 'aSize' if 'side' in args else n if int(level) == 2 else k
  sizeX = n if level == 2.1 else m
  sizeY = m if level == 2.1 else n
  # process all the arguments
  for a in args:
    if a in ['side']:
      body += processFlagArg(a, 'Side', name in ['trmm', 'trsm'], name in ['trmm', 'trsm'])
      pencilArgs.append('int aSize')
      pencilCallArgs.append('*aSize')
      body += '  int* aSize = side == LEFT ? m : n;\n'
    elif a in ['uplo']:
      body += processFlagArg(a, 'Uplo', int(level) == 2 and not intersect(['AP', 'AT', 'ABT'], args), False)
    elif a in ['tA', 'tB', 'tC', 'tAB']:
      body += processFlagArg(a, 'Trans', False, False)
    elif a in ['diag']:
      body += processFlagArg(a, 'Diag', not 'triangular' in args, name in ['trmm', 'trsm'])
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
      processArrayArg(a, 'Vector', wType.valueType, 'inc', [size, '@SIGN@inc' + a], isConstArrayArg(name, a))
    # Process matrices
    elif a in ['A', 'B', 'C']:
      nRow = k if a == 'B' else nRowA if a == 'A' else m
      nCol = nColA if a == 'A' else n
      if 't' + a not in args:
        defaultTrans = 'tA' if a in ['B'] and name in ['syr2k', 'her2k'] else 'NOTRANS'
        body += '  int t' + a + ' = ' + defaultTrans + ';\n'
      processArrayArg(a, 'Matrix', wType.valueType, 'ld', [nRow, nCol], isConstArrayArg(name, a))
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
  # rotg is different from the rest
  if name == 'rotg':
    processRotgArg(wType)
  # generate transpositions
  if name in ['trmm', 'trsm']:
    body += '  uplo ^= (tA & TRANS);\n'
  if name in ['trmm', 'trsm']:
    body += genCondTranspose('uplo == LOWER', intersect(['A', 'B', 'side'], args))
  elif intersect([name], ['symm', 'hemm']) :
    body += genCondTranspose('side == RIGHT', ['C'])
  # Handle hermitian matrices
  if 'fillDiagIm' in args:
    body += '  for(int i=0; i<*n; i++) {\n'
    for name in 'AC':
      if name + 'T' in args:
        body += '    ' + name + 'T[i*(1 + *ld' + name + 'T)].Im = 0;\n'
    if 'AP' in args:
      body += '    if(uplo)\n'
      body += '      AP[(i*(2*(*n)-i+1))/2].Im = 0;\n'
      body += '    else\n'
      body += '      AP[(i*(i+3))/2].Im = 0;\n'
    body += '  }\n'
  # Generate the wrapper
  aCases = getArrayCases(args)
  decl = ''
  pencilFuncName = 'pencil_' + wType.name
  callBeg = initCall(name, wType.returnType, pencilFuncName, args)
  callEnd = endCall(pencilCallArgs, name)
  body += '  switch(' + genSwitchValue(args, mCases, wType.isComplex) + ') {\n'
  for mc in mCases:
    if (not wType.isComplex) and ('c' in mc or 'u' in mc):
      continue
    for ac in aCases:
      arglist = ', '.join(pencilArgs)
      decl += fReturnType + ' ' + pencilFuncName + getCaseSuffix(mc, ac)
      decl += '(' + setArraysizeSign(arglist, ac) + ');\n'
      body += genSwitchCase(mc, ac, wType.isComplex, callBeg, callEnd)
  body += '  }\n'
  wrapper = decl + wType.returnType + ' ' + wType.name + '_(' + ', '.join(cwrapArgs) + ') {\n'
  wrapper += body
  if wType.returnType == 'void':
    wrapper += '  return;\n'
  else:
    wrapper += '  return out;\n'
  wrapper += '}\n'
  # Save arguments for use during PENCIL wrapper generation
  pencilArgsMap[wType.name] = pencilArgs
  voblaArgsMap[wType.name] = voblaArgs
  voblaCallArgsMap[wType.name] = voblaCallArgs
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
  """Generate C wrappers for all BLAS functions."""
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
#####  PENCIL Wrappers  #####
#############################

def genPencilHeader():
  s = """
struct ComplexFloat {
  float Re;
  float Im;
};

struct ComplexDouble {
  double Re;
  double Im;
};

// Storage

struct ArrayView {
  int base_size0;
  int base_size1;
};

struct PackedTriangularStorage {
  int n;
};

// Views

struct VectorView {
  struct ArrayView storage;
  int view_size0;
  int offset0;
  int offset1;
};

struct MatrixView {
  struct ArrayView storage;
  int view_size0;
  int view_size1;
  int offset0;
  int offset1;
};

struct TriangleView {
  struct ArrayView storage;
  int n;
  int offset0;
  int offset1;
};

struct BandTriangleView {
  struct ArrayView storage;
  int n;
  int k;
  int offset0;
  int offset1;
};

struct BandMatrixView {
  struct ArrayView storage;
  int m;
  int n;
  int kl;
  int ku;
};

struct PackedTriangleView {
  struct PackedTriangularStorage storage;
  int n;
  int offset0;
  int offset1;
};

"""
  return s

def setArraysizeSign(size, case):
  """Replace occurrences of @SIGN@ with a minus if array is accessed in reverse."""
  for ac in case:
    sign = '-' if ac == 'r' else ''
    size = size.replace('@SIGN@', sign, 1)
  return size

def genPencilVectorView(arg, case, size):
  viewname = arg + "_view"
  view = ''
  view += '  struct VectorView ' + viewname + ';\n'
  view += '  ' + viewname + '.storage.base_size0 = ' + size + ';\n'
  view += '  ' + viewname + '.storage.base_size1 = abs(inc' + arg + ');\n'
  view += '  ' + viewname + '.view_size0 = ' + size + ';\n'
  view += '  ' + viewname + '.offset0 = ' + \
          ('0' if case == 'n' else size + ' - 1') + \
          ';\n'
  view += '  ' + viewname + '.offset1 = 0' + ';\n'
  return view

def genPencilMatrixView(arg, case, nRows, nCols):
  viewname = arg + "_view"
  dim0 = nCols if case == 't' else nRows
  dim1 = nRows if case == 't' else nCols
  view = ''
  view += '  struct MatrixView ' + viewname + ';\n'
  view += '  ' + viewname + '.storage.base_size0 = ' + dim0 + ';\n'
  view += '  ' + viewname + '.storage.base_size1 = ld' + arg + ';\n'
  view += '  ' + viewname + '.view_size0 = ' + dim0 + ';\n'
  view += '  ' + viewname + '.view_size1 = ' + dim1 + ';\n'
  view += '  ' + viewname + '.offset0 = 0;\n'
  view += '  ' + viewname + '.offset1 = 0;\n'
  return view

def getPencilRotgView():
  view = ''
  view += '  struct VectorView one_view;\n'
  view += '  one_view.storage.base_size0 = 1;\n'
  view += '  one_view.storage.base_size1 = 1;\n'
  view += '  one_view.view_size0 = 1;\n'
  view += '  one_view.offset0 = 0;\n'
  view += '  one_view.offset1 = 0;\n'
  return view

def genPencilTriangularMatrixView(n, name):
  view = ''
  view += '  struct TriangleView ' + name + 'T_view;\n'
  view += '  ' + name + 'T_view.storage.base_size0 = ' + n + ';\n'
  view += '  ' + name + 'T_view.storage.base_size1 = ld' + name + 'T;\n'
  view += '  ' + name + 'T_view.n = ' + n + ';\n'
  view += '  ' + name + 'T_view.offset0 = 0;\n'
  view += '  ' + name + 'T_view.offset1 = 0;\n'
  return view

def getPencilBandMatrixView():
  view = ''
  view += '  struct BandMatrixView A_view;\n'
  view += '  A_view.storage.base_size0 = n;\n'
  view += '  A_view.storage.base_size1 = ldA;\n'
  view += '  A_view.m = n;\n'
  view += '  A_view.n = m;\n'
  view += '  A_view.kl = ku;\n'
  view += '  A_view.ku = kl;\n'
  return view

def getPencilTriangularBandMatrixView():
  view = ''
  view += '  struct BandTriangleView ABT_view;\n'
  view += '  ABT_view.storage.base_size0 = n;\n'
  view += '  ABT_view.storage.base_size1 = ldABT;\n'
  view += '  ABT_view.n = n;\n'
  view += '  ABT_view.k = k;\n'
  view += '  ABT_view.offset0 = 0;\n'
  view += '  ABT_view.offset1 = 0;\n'
  return view

def getPencilPackedMatrixView():
  view = ''
  view += '  struct PackedTriangleView AP_view;\n'
  view += '  AP_view.storage.n = n;\n'
  view += '  AP_view.n = n;\n'
  view += '  AP_view.offset0 = 0;\n'
  view += '  AP_view.offset1 = 0;\n'
  return view

def getPencilMatrixCase(name, mc, i):
  # Default: no transpose
  case = 'n'
  if name in ['symm', 'hemm', 'ger', 'geru', 'gerc']:
    # There is no user-visible transpose flag for these
    case = 't'
  elif name in ['gemv', 'trmm', 'trsm']:
    # Always transpose
    case = 't'
  elif i < len(mc):
    case = mc[i]
  return case

def genPencilViews(name, level, args, mc, ac):
  """Generate View structure initialization statements."""
  views = ''
  acIdx = 0
  mcIdx = 0
  # Map size variables depending on what other size arguments are present
  n = 'n'
  m = 'm' if 'm' in args else 'n'
  k = 'k' if 'k' in args else 'm'

  # rotg is very different
  if name == 'rotg':
    return getPencilRotgView()

  if 'triangular' in args or intersect(['AP', 'AT', 'ABT', 'CT'], args) and level == 2.2:
    acIdx += 1

  for arg in args:
    nRowA = 'aSize' if 'side' in args else m
    nColA = 'aSize' if 'side' in args else n if int(level) == 2 else k

    # Process arrays
    if arg in ['X', 'Y']:
      size = 'm' if 'm' in args and acIdx > 0 else 'n'
      if name in ['gemv', 'gbmv'] and mc != 't':
        size = 'm' if size == 'n' else 'n'
      if name in ['ger', 'geru', 'gerc']:
        size = 'm' if size == 'n' else 'n'
      views += genPencilVectorView(arg, ac[acIdx], size)
      acIdx += 1
    elif arg in ['A', 'B', 'C']:
      nRows = k if arg == 'B' else nRowA if arg == 'A' else m
      nCols = nColA if arg == 'A' else n
      views += genPencilMatrixView(arg, getPencilMatrixCase(name, mc, mcIdx), nRows, nCols)
      mcIdx += 1
    elif arg in ['AB']:
      views += getPencilBandMatrixView()
    elif arg in['AT']:
      views += genPencilTriangularMatrixView(nRowA, 'A')
      acIdx += 1
    elif arg in['CT']:
      views += genPencilTriangularMatrixView(n, 'C')
      acIdx += 1
    elif arg in['ABT']:
      views += getPencilTriangularBandMatrixView()
      acIdx += 1
    elif arg in['AP']:
      views += getPencilPackedMatrixView()
      acIdx += 1
  return views

def genPencilWrapper(name, wType, level, args, mCases):
  aCases = getArrayCases(args)
  prototypes = '// Prototypes of VOBLA-generated PENCIL functions\n'
  pwrappers = ''
  fReturnType = wType.returnType
  # C wrapper always returns void following f2c convention, but PENCIL wrapper returns complex struct
  if 'out' in args:
    fReturnType = wType.valueType
  for mc in mCases:
    if (not wType.isComplex) and ('c' in mc or 'u' in mc):
      continue
    for ac in aCases:
      funcname = wType.name + getCaseSuffix(mc, ac)
      prototypes += fReturnType + ' ' + funcname
      prototypes += '(' + ', '.join(voblaArgsMap[wType.name]) + ');\n'
      arglist = ', '.join(pencilArgsMap[wType.name])
      pwrappers += fReturnType + ' pencil_' + funcname
      pwrappers += '(' + setArraysizeSign(arglist, ac) + ') {\n'
      pwrappers += '#pragma scop\n'
      pwrappers += '  {\n'
      pwrappers += genPencilViews(name, level, args, mc, ac)
      # Generate call to VOBLA-generated PENCIL function
      pwrappers += '  '
      if fReturnType != 'void':
        pwrappers += 'return '
      pwrappers += funcname
      pwrappers += '(' + ', '.join(voblaCallArgsMap[wType.name]) + ');\n'
      pwrappers += '  }\n'
      pwrappers += '#pragma endscop\n'
      pwrappers += '}\n'
  wrapper = prototypes + '\n' + pwrappers + '\n'
  return wrapper

def genPencilWrappersForFunction(f):
  """Generate all PENCIL wrappers for a BLAS function, e.g. saxpy_nn, saxpy_rn, daxpy_nn, ..."""
  if f[3] == 3:
    mCases = f[5]
  elif f[0] in ['gemv', 'trmv', 'trsv', 'gbmv']:
    mCases = ['t', 'n', 'c']
  elif f[0] in ['tpmv', 'tpsv', 'tbmv', 'tbsv']:
    mCases = ['t', 'n', 'u']
  else:
    mCases = ['']
  # Generate versions for each data type
  wrapper = ""
  for t in f[1]:
    wrapper += genPencilWrapper(f[0], WrapperType(f[0], t, f[2]), f[3], f[4], mCases)
  return wrapper

def generateAllPencil():
  """Generate PENCIL wrappers for all BLAS functions."""
  files = dict()
  # generate the files
  for lvl in [1, 2, 3]:
    files[lvl] = open('blas' + str(lvl) + '/pwrapper.c', 'w')
    files[lvl].write(genPencilHeader())
  # generate the wrappers
  for f in functions:
    wrappers = genPencilWrappersForFunction(f)
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
  global pencilArgsMap
  global voblaArgsMap, voblaCallArgsMap
  pencilArgsMap = {}
  voblaArgsMap = {}
  voblaCallArgsMap = {}
  if options.generate_all:
    generateAll()
    generateAllPencil()
  else:
    for f in args:
      # C wrapper
      outfile = open(f + "-wrapper.c", "w")
      outfile.write(generateIncludes())
      outfile.write(genWrappersForFunction(findFunction(f)))
      outfile.close()
      # PENCIL wrappers
      outfile = open(f + "-pwrapper.c", "w")
      outfile.write(genPencilHeader())
      outfile.write(genPencilWrappersForFunction(findFunction(f)))
      outfile.close()

if __name__ == "__main__":
  main()
