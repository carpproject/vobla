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

package com.arm.carp.frontends.vobla

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import scala.collection.mutable.Queue
import com.arm.carp.{ pencil => Pencil }
import com.arm.carp.{ vobla => Vobla }
import com.arm.carp.frontends.vobla.VoblaImplicits._
import com.arm.carp.frontends.vobla.OpMonad._
import com.arm.carp.vobla.ComparisonOp

private class FunctionTransformer(
  valueType: Vobla.ValueVersion.ValueVersion,
  structs: StructProvider,
  functionVersions: MultiMap[Vobla.FunctionVersion, Pencil.Function],
  additionalFunctions: Queue[(Vobla.FunctionVersion, Pencil.Function, FunctionTransformer)],
  variables: HashMap[Vobla.Variable, View] = HashMap[Vobla.Variable, View](),
  yieldOp: Option[List[View] => Pencil.Operation] = None,
  isParallel: Boolean = true,
  methodView: Option[InterfaceView] = None)
  extends ValueTransformer(valueType, structs, variables) {

  type YieldCallback = List[View] => Pencil.Operation

  private def rotate[T] = (_: List[T]) match {
    case h :: q => q ::: List(h)
    case List() => List()
  }

  private val getRe = (in: ScalarView) => in.value.expType match {
    case _: Pencil.StructType => Pencil.ScalarStructSubscription(in.value, 0)
    case _: Pencil.ScalarType => in.value
  }

  private def getIm(in: ScalarView, checkConj: Boolean = true) = in.value.expType match {
    case _: Pencil.StructType if checkConj && in.conjugated =>
      Pencil.UnaryMinusExpression(Pencil.ScalarStructSubscription(in.value, 1))
    case _: Pencil.StructType => Pencil.ScalarStructSubscription(in.value, 1)
    case stype: Pencil.ScalarType => getScalarZero(stype)
  }

  private def createScalar2OpExpr(
      notComplex: (Pencil.ScalarExpression, Pencil.ScalarExpression) => Pencil.ScalarExpression,
      complex: (ScalarView, ScalarView) => OpMonad[(Pencil.ScalarExpression, Pencil.ScalarExpression)])
      (exp0: ScalarView, exp1: ScalarView) = {
    if(!isComplex(exp0.value.expType, exp1.value.expType)) OpMonad(ScalarView(notComplex(exp0.value, exp1.value)))
    else !!(createComplex.tupled)(complex(exp0, exp1))
  }

  private val createComplex = (re: Pencil.ScalarExpression, im: Pencil.ScalarExpression) => {
    val tmp = Pencil.ScalarVariable(ComplexType, "var", None)
    val init = List(
      createAssignment(Pencil.ScalarStructSubscription(tmp, 0), re),
      createAssignment(Pencil.ScalarStructSubscription(tmp, 1), im))
    OpMonad(ScalarView(tmp), init)
  }

  private val createPlusExpression = createScalar2OpExpr((e0, e1) => createPlus(e0, e1),
      (e0, e1) => (createPlus(getRe(e0), getRe(e1)), createPlus(getIm(e0), getIm(e1))))_

  private val createMinusExpression = createScalar2OpExpr((e0, e1) => createMinus(e0, e1),
      (e0, e1) => (createMinus(getRe(e0), getRe(e1)), createMinus(getIm(e0), getIm(e1))))_

  private val createUMinusExpression = (op1: ScalarView) => isComplex(op1.value.expType) match {
    case false => OpMonad(ScalarView(Pencil.UnaryMinusExpression(op1.value)))
    case true => createComplex(Pencil.UnaryMinusExpression(getRe(op1)), Pencil.UnaryMinusExpression(getIm(op1)))
  }

  private val createMultExpression = createScalar2OpExpr((e0, e1) => createMul(e0, e1), createComplexMult)_

  private val createComplexMult = (e0: ScalarView, e1: ScalarView) =>
    OpMonad((createMinus(createMul(getRe(e0), getRe(e1)), createMul(getIm(e0), getIm(e1))),
     createPlus(createMul(getIm(e0), getRe(e1)), createMul(getRe(e0), getIm(e1)))))

  private val createDivExpression = createScalar2OpExpr((e0, e1) => createDiv(e0, e1), createComplexDiv)_

  private val createComplexDiv = (e0: ScalarView, e1: ScalarView) => {
    val coeff = Pencil.ScalarVariable(RealType, "var", None)
    val (re0, im0, re1, im1) = (getRe(e0), getIm(e0), getRe(e1), getIm(e1))
    val re = createDiv(createPlus(createMul(re0, re1), createMul(im0, im1)), coeff)
    val im = createDiv(createMinus(createMul(re1, im0), createMul(re0, im1)), coeff)
    val coeffVal = createPlus(createMul(re1, re1), createMul(im1, im1))
    OpMonad((re, im), createAssignment(coeff, coeffVal))
  }

  private def createPencilBinaryExp[T](op: (Pencil.ScalarExpression, Pencil.ScalarExpression) => T)
    (e0: Pencil.ScalarExpression, e1: Pencil.ScalarExpression) = {
    val _type = getMaxType(e0.expType, e1.expType)
    op(convertScalar(e0, _type), convertScalar(e1, _type))
  }

  private def createPlus = createPencilBinaryExp(Pencil.PlusExpression)_
  private def createMinus = createPencilBinaryExp(Pencil.MinusExpression)_
  private def createMul = createPencilBinaryExp(Pencil.MultExpression)_
  private def createDiv = createPencilBinaryExp(Pencil.DivExpression)_
  private def createAssignment(lValue: Pencil.ScalarExpression with Pencil.LValue, rValue: Pencil.ScalarExpression) =
    new Pencil.AssignmentOperation(lValue, convertScalar(rValue, lValue.expType))

  private val createAssignment: (LScalarView, ScalarView) => Pencil.Operation = (op1, op2) => {
    if (!isComplex(op1.value.expType)) {
      assert(!isComplex(op2.value.expType), (op1, op2), "can not assign Complex rvalue to Real lvalue")
      createAssignment(op1.value,op2.value)
    } else if (isComplex(op1.value.expType) && isComplex(op2.value.expType)) {
      if (op1.conjugated == op2.conjugated) {
        createAssignment(op1.value, op2.value)
      } else {
        val lvalueRe = Pencil.ScalarStructSubscription(op1.value, 0)
        val lvalueIm = Pencil.ScalarStructSubscription(op1.value, 1)
        new Pencil.BlockOperation(List(
          createAssignment(lvalueRe, getRe(op2)),
          createAssignment(lvalueIm, Pencil.UnaryMinusExpression(getIm(op2, false)))))
      }
    } else {
      val lvalueRe = Pencil.ScalarStructSubscription(op1.value, 0)
      val lvalueIm = Pencil.ScalarStructSubscription(op1.value, 1)
      new Pencil.BlockOperation(List(createAssignment(lvalueRe, op2.value), createAssignment(lvalueIm, getScalarZero(RealType))))
    }
  }

  private def transformScalarVariable(in: Vobla.Variable) = varTable(in) match {
    case v: ScalarView => v
    case v => ice(v, "ScalarView expected")
  }

  private def transformArrayVariable(in: Vobla.Variable) = varTable(in) match {
    case view: SimpleArrayView => view
    case res => ice(res, "unexpected expression(variable or offset expected)")
  }

  /**
   * Convert VOBLA array constant to PENCIL array constant (preserving the value).
   *
   * Since PENCIL only allows array constant in initializers, VOBLA array constants
   * are transformed into variables initialized by corresponding constant. For example:
   * let A = [1 2] + [3 4];
   *
   * will be transformed to:
   *
   * const int var_4[2] = {1,2};
   * const int var_5[2] = {1,2};
   * int A_2[2];
   *
   * And A will be initialized in a loop.
   *
   * This method should not be used for transforming initialization constants, since
   * it would generate unnecessary data transfers:
   *
   * let A = [1 2];
   * will be translated to
   *
   * int A_2[2];
   * const int var_4[2] = {1,2};
   * // and A initialization in a for loop.
   *
   * instead of:
   * int A_2[2] = {1,2};
   *
   * This is taken into account by the VOBLA transformer.
   *
   * @param in - VOBLA array constant.
   * @return New PENCIL array variable, initialized with transformed array constant.
   */
  private def transformArrayConstant(in: Vobla.ArrayConstant): Pencil.ArrayConstant = {
    val elems = in.values.map {
      case c: Vobla.ArrayConstant => transformArrayConstant(c)
      case c => transformScalarConstant(c)
    }
    Pencil.ArrayConstant(Pencil.ArrayType(elems(0).expType, Pencil.IntegerConstant(IndexType, elems.size)), elems)
  }

  /**
   * Convert VOBLA array constant to PENCIL array constant (preserving the value).
   * @param in - VOBLA array constant.
   * @return PENCIL array constant.
   */
  private def createArrayConstant(in: Vobla.ArrayConstant): SimpleArrayView = {
    val cst = transformArrayConstant(in)
    val (baseType, sizes) = decomposeArrayType(cst.expType)
    createArrayVariable(baseType, sizes, true, Some(cst))._1
  }

  private def decomposeArrayType(t: Pencil.ArrayType): (Pencil.ScalarType, List[Pencil.ScalarExpression]) = t.base match {
    case st: Pencil.ScalarType => (st, List(t.range))
    case at: Pencil.ArrayType =>
      val (bt, s) = decomposeArrayType(at)
      (bt, t.range :: s)
  }

  /**
   * Convert VOBLA scalar constant to PENCIL scalar constant (preserving the value).
   * @param in - VOBLA scalar constant.
   * @return PENCIL scalar constant.
   */
  private def transformScalarConstant(in: Vobla.Constant) = in match {
    case Vobla.IndexConstant(i) => new Pencil.IntegerConstant(IndexType, i)
    case Vobla.RealConstant(f) => new Pencil.FloatConstant(RealType, f)
    case _ => ice(in, "scalar constant expected")
  }

  private def transformSum(in: Vobla.SumExpression): OpMonad[LScalarView] = {
    val accum = createScalarVariable("accum", in.expType.setAssignable(true))
    val init = createAssignment(accum, ScalarView(getScalarZero(RealType)))
    val op = () => !!!(createAssignment)(accum, !!(createPlusExpression)(accum, transformScalarExpression(in.base)))
    val body = transformRange(in.iteration.range, false, createYieldOp(op, in.iteration.variables))
    OpMonad(accum, List(init, body))
  }

  private def updateOffsets(view: ArrayView, idx: Pencil.ScalarExpression) =
    (view.offsets, view.factors.head).zipped.map {
      case (offset, 0) => offset
      case (offset, factor) => createPlus(offset, createMul(transformIndexCst(factor), idx))
    }

  private def transformSlice = (baseView: ArrayView, lower: ScalarView, upper: ScalarView) => {
    val newOffsets = updateOffsets(baseView, lower.value)
    val newSizes = baseView.sizes.tail ::: List(createPlus(createMinus(upper.value, lower.value), IndexOne))
    val newFactors = baseView.factors.tail ::: List(baseView.factors.head)
    baseView.copy(sizes = newSizes, offsets = newOffsets, factors = newFactors)
  }

  private val transformIdx = (base: ArrayView, idx: ScalarView) =>
    base.copy(sizes = base.sizes.tail, offsets = updateOffsets(base, idx.value), factors = base.factors.tail)

  private val transformFullSlice = (base: ArrayView) => base.copy(sizes = rotate(base.sizes), factors = rotate(base.factors))

  private def transformArrayOffset(exp: Vobla.OffsetExpression): OpMonad[ArrayView] = {
    exp.offsets.foldLeft(transformArrayExpression(exp.base)) {
      case (base, Vobla.FullSlice()) => !!(transformFullSlice)(base)
      case (base, Vobla.Idx(idx)) => !!(transformIdx)(base, transformScalarExpression(idx))
      case (base, slice: Vobla.Slice) =>
        !!(transformSlice)(base, transformScalarExpression(slice.low), transformScalarExpression(slice.up))
      case e => ice(e, "Bad offset")
    }
  }

  private val applyIdx = (view: ArrayView, origIters: List[Pencil.ScalarExpression]) => {
    val finalView = origIters.foldLeft(view)((v, idx) => transformIdx(v, ScalarView(idx)))
    assert(finalView.dimension == 0, finalView, "Scalar expected but array found")
    val scalar = finalView match {
      case v: SimpleArrayView =>
        val array = finalView.offsets.init.foldLeft[Pencil.ArrayExpression](v.storage)((array, offset) =>
          Pencil.ArrayIdxExpression(array, offset))
        OpMonad(new LScalarView(Pencil.ScalarIdxExpression(array, finalView.offsets.last), view.conjugated))
      case view: ComposedArrayView => view()
    }
    view.arrayPart match {
      case Vobla.ArrayPart.Full => scalar
      case Vobla.ArrayPart.Re => !!(ScalarView(_: Pencil.ScalarExpression))(!!(getRe)(scalar))
      case Vobla.ArrayPart.Im => !!(ScalarView(_: Pencil.ScalarExpression))(!!(getIm(_: ScalarView))(scalar))
      case Vobla.ArrayPart.Zero => OpMonad(ScalarView(IndexZero))
    }
  }

  private val convertForCall: (ScalarView, Pencil.ScalarType) => OpMonad[Pencil.ScalarExpression] = (arg, expected) => {
    if (arg.value.expType.convertible(expected)) convertScalar(arg.value, expected)
    else {
      assert(arg.value.expType.isNumeric && expected == ComplexType, (arg, expected), "Real to Complex conversion expected")
      val tmp = Pencil.ScalarVariable(ComplexType, "var", None)
      val lvalueRe = Pencil.ScalarStructSubscription(tmp, 0)
      val lvalueIm = Pencil.ScalarStructSubscription(tmp, 1)
      val init = new Pencil.BlockOperation(List(
        createAssignment(lvalueRe, arg.value),
        createAssignment(lvalueIm, getScalarZero(RealType))))
      OpMonad(tmp, init)
    }
  }

  private def getSimpleView = (_: View) match {
    case v: SimpleView => OpMonad(v)
    case rView: ComposedArrayView =>
      val lView = createArrayVariable(rView.baseType, rView.sizes)._1
      OpMonad(lView, copyArray(lView, rView))
    case v: ComposedInterfaceView =>
      val sizes: OpMonad[List[Pencil.ScalarExpression]] = List.tabulate(v.dim)((i: Int) => getSize(i+1)(v))
      val lView = createArrayVariable(v.scalarType, sizes.value)._1
      val init = transformArrayScalarAssign(createAssignment, false)(lView, RScalarView(IndexZero))
      val assign = transformArrayAssignment(createAssignment, false, true)(lView, v)
      OpMonad(lView, init :: assign :: sizes.ops)
      // TODO: Copy the sparse structure instead of creating a dense one
  }

  private def createConcreteArrayView(oldView: ArrayView) = {
    val baseDim = oldView.offsets.size
    val viewType = structures.getArrayViewStruct(baseDim, oldView.dimension)
    val struct = Pencil.ScalarVariable(viewType, "var", None)
    val (baseSizes, sizes, offsets) = structures.getArrayViewMembers(struct, baseDim, oldView.dimension)
    val OpMonad(simpleView, ops0) = oldView match {
      case v: SimpleArrayView => OpMonad(v)
      case rView: ComposedArrayView =>
        val lView = createArrayVariable(rView.baseType, rView.sizes)._1
        OpMonad(lView, copyArray(lView, rView))
      }
    val view = new ConcreteArrayView(struct, simpleView.conjugated, simpleView.restrict, simpleView.arrayPart,
        simpleView.storage, sizes, baseSizes, offsets, simpleView.factors)
    val ops1 = new Pencil.BlockOperation(
      (sizes, simpleView.sizes).zipped.map((a, b) => createAssignment(a, b))
        ::: (baseSizes, simpleView.baseSizes).zipped.map((a, b) => createAssignment(a, b))
        ::: (offsets, simpleView.offsets).zipped.map((a, b) => createAssignment(a, b)))
    OpMonad(view, ops0 :+ ops1)
  }

  private def createConcreteInterfaceView(inView: SimpleInterfaceView) = inView.voblaType match {
    case t: Vobla.VoblaType =>
    val concreteView = structures.getType(t, valueType, inView.conjugated, inView.restrict, inView.specType)
    val assignments = concreteView.varTable.flatMap{ case (voblaVar, exp) => (exp, inView.varTable(voblaVar)) match {
      case (exp: Pencil.ScalarExpression with Pencil.LValue, oldExp: Pencil.ScalarExpression) => Some(createAssignment(exp, oldExp))
      case (_: Pencil.ScalarExpression, _) | (_, _: Pencil.ScalarExpression) => ice(exp, "Invalid interface member")
      case _ => None
    }}.toList
    OpMonad(concreteView, assignments)
    case t: Vobla.VoblaView =>
      val OpMonad(subView, ops) = getConcreteView(inView.varTable(t.base))
      OpMonad(ConcreteInterfaceView(inView.dimension, subView.view, subView.storages, Map(t.base -> subView), t, inView.specType,
          inView.conjugated, inView.conjugated), ops)
  }

  private val getConcreteView: View => OpMonad[ConcreteView] = _ match {
    case view: ConcreteView => view
    case view: ArrayView => createConcreteArrayView(view)
    case view: VirtualInterfaceView => createConcreteInterfaceView(view)
  }

  private def updateRestrict(views: ArrayBuffer[Vobla.ArgVersion], idx: HashMap[Pencil.ArrayVariable, Int], variable: Pencil.ArrayVariable) = {
    idx.get(variable) match {
      case None => idx.put(variable, views.size - 1)
      case Some(-1) =>
      case Some(index) =>
        idx.put(variable, -1)
        val elem = views(index) match {
          case v: Vobla.ArrayVersion => v.copy(restrict = false)
          case v: Vobla.InterfaceVersion => v.copy(restrict = false)
          case v: Vobla.ScalarVersion => ice(v, "unexpected scalar")
        }
        views.update(index, elem)
    }
  }

  def composeFactors(view: ArrayView, f: List[List[Int]]) = {
    val viewFactors = view.factors.transpose
    val newFactors = f.map { upperFactors => viewFactors.map { lowerFactors => (upperFactors, lowerFactors).zipped.map((i, j) => i * j).sum } }
    val offsetsAdditions = (f.transpose, view.sizes).zipped.map { (factors, s) =>
      val c = factors.count(i => i < 0)
      if (c == 0) None else Some(createMul(transformIndexCst(c), createMinus(s, IndexOne)))
    }
    val newOffsets = (viewFactors, view.offsets).zipped.map { (lowerFactors, offset) =>
      (lowerFactors, offsetsAdditions).zipped.foldLeft(offset) {
        case (s, (_, None) | (0, _)) => s
        case (s, (i, Some(add))) => createPlus(s, createMul(transformIndexCst(i), add))
      }
    }
    (newFactors, newOffsets)
  }

  private def extractArrayView(dim: Int, factors: List[List[Int]], sizes: List[Pencil.ScalarExpression] => List[Pencil.ScalarExpression])
    (baseView: ArrayView) = {
    assert(baseView.dimension == dim, baseView, "Invalid dimension : " + baseView.dimension + " (expected " + dim + ")")
    val (newFactors, newOffsets) = composeFactors(baseView, factors)
    baseView.copy(sizes = sizes(baseView.sizes), offsets = newOffsets, factors = newFactors)
  }

  private val transformDiag = extractArrayView(2, List(List(1, 1)), s => List(Pencil.IntrinsicCallExpression("min", s)))_
  private val transformAntiDiag = extractArrayView(2, List(List(1, -1)), s => List(Pencil.IntrinsicCallExpression("min", s)))_
  private val transformTransposed = extractArrayView(2, List(List(0, 1), List(1, 0)), s => s.reverse)_
  private val transformReveresed = extractArrayView(1, List(List(-1)), s => s)_
  private val transformArrayConj = (baseView: ArrayView) => baseView.copy(conjugated = true)

  private def transformCallExpression(call: Vobla.CallExpression) = {
    val name = call.function.decl.name
    val argsTypes = call.function.decl.args.map(a => a.specialize(call.specType))
    val arglist = ArrayBuffer[Pencil.Expression]()
    val argVersions = ArrayBuffer[Vobla.ArgVersion]()
    val restrictVars = HashMap[Pencil.ArrayVariable, Int]()
    val init = call.args.zipWithIndex.flatMap {
      case (arg, i) => arg.expType match {
        case t: Vobla.ScalarType =>
          val expression = transformScalarExpression(arg)
          val t = argsTypes(i) match {
            case _: Vobla.RealType => RealType
            case _: Vobla.ComplexType => ComplexType
            case _: Vobla.ValueType => ValueType
            case _: Vobla.IndexType => IndexType
          }
          val convertedExp = !!(convertForCall)(expression, t)
          arglist += convertedExp.value
          argVersions += expression.value.version
          convertedExp.ops
        case _: Vobla.ArrayType | _: Vobla.InterfaceType =>
          val view = transformObjectExpression(arg)
          val OpMonad(concreteView, ops) = !!(getConcreteView)(view)
          arglist ++= concreteView.view :: concreteView.storages
          argVersions += concreteView.version
          concreteView.storages.foreach(v => updateRestrict(argVersions, restrictVars, v))
          ops
      }
    }
    val version = new Vobla.FunctionVersion(getValueVersion(call.specType), call.function, argVersions.toList)
    val function = getFunction(version)
    OpMonad(ScalarView(Pencil.CallExpression(function, arglist)), init)
  }

  private def getValueVersion(t: Vobla.ScalarType) = (t, valueType) match {
    case (_: Vobla.RealType, Vobla.ValueVersion.FLOAT | Vobla.ValueVersion.COMPLEX_FLOAT) => Vobla.ValueVersion.FLOAT
    case (_: Vobla.RealType, Vobla.ValueVersion.DOUBLE | Vobla.ValueVersion.COMPLEX_DOUBLE) => Vobla.ValueVersion.DOUBLE
    case (_: Vobla.ValueType, _) => valueType
    case (_: Vobla.ComplexType, Vobla.ValueVersion.FLOAT | Vobla.ValueVersion.COMPLEX_FLOAT) => Vobla.ValueVersion.COMPLEX_FLOAT
    case (_: Vobla.ComplexType, Vobla.ValueVersion.DOUBLE | Vobla.ValueVersion.COMPLEX_DOUBLE) => Vobla.ValueVersion.COMPLEX_DOUBLE
  }

  private def transformIntrinsicCall(call: Vobla.IntrinsicCallExpression) = {
    val argList = call.args.map(transformScalarExpression(_))
    val name = (call.name, call.args(0).expType) match {
      case ("min", _: Vobla.RealType) => "fmin"
      case ("max", _: Vobla.RealType) => "fmax"
      case ("abs", _: Vobla.RealType) => "fabs"
      case (s, _) => s
    }
    val argValues = argList.map(!!((a: ScalarView) => a.value))
    !!((l: List[Pencil.ScalarExpression]) => ScalarView(Pencil.IntrinsicCallExpression(name, l)))(argValues)
  }

  private def getFunction(version: Vobla.FunctionVersion) = functionVersions.get(version) match {
    case Some(functions) if !functions.isEmpty => functions.head
    case _ =>
      val transformer = new FunctionTransformer(version.valueVersion, structs, functionVersions, additionalFunctions)
      val function = transformer.transformFunctionDeclaration("auto_function", version, true)
      functionVersions.addBinding(version, function)
      additionalFunctions.enqueue((version, function, transformer))
      function
  }

  private def transformScalarOffset(e: Vobla.OffsetExpression) = {
    val base = transformArrayOffset(Vobla.OffsetExpression(e.base, e.offsets.init, null))
    val offset = e.offsets.last match {
      case Vobla.Idx(idx) => transformScalarExpression(idx)
      case _ => ice(e, "array subscription expected")
    }
    !!(applyIdx)(base, List(!!((v: ScalarView) => v.value)(offset)))
  }

  private val transformScalarExpression: Vobla.Expression => OpMonad[ScalarView] = _ match {
    case variable: Vobla.Variable => transformScalarVariable(variable)
    case constant: Vobla.Constant => ScalarView(transformScalarConstant(constant))
    case Vobla.VecMultExpression(e0, e1, t) =>
      !!(transformVecMult)(transformArrayExpression(e0), transformArrayExpression(e1), t)
    case e: Vobla.BaseMethodCallExpression =>
      val obj = (e, methodView) match {
        case (e: Vobla.MethodCallExpression, _) => transformObjectExpression(e.base)
        case (_, Some(obj)) => OpMonad(obj)
        case e => ice(e, "Invalid self method call")
      }
      !!(transformMethodCallExpression)(obj, e) match {
        case OpMonad(s: ScalarView, ops) => OpMonad(s, ops)
        case e => ice(e, "OpMonad[View] expected")
      }
    case call: Vobla.IntrinsicCallExpression => transformIntrinsicCall(call)
    case call: Vobla.CallExpression => transformCallExpression(call)
    case e: Vobla.OffsetExpression => transformScalarOffset(e)
    case sum: Vobla.SumExpression => transformSum(sum)
    case e: Vobla.OneOpExpression => !!(e match {
      case _: Vobla.UMinusExpression => createUMinusExpression
      case _: Vobla.ImExpression => (base: ScalarView) => OpMonad(ScalarView(getIm(base)))
      case _: Vobla.ReExpression => (base: ScalarView) => OpMonad(ScalarView(getRe(base)))
      case _: Vobla.ConjExpression => (base: ScalarView) => OpMonad(ScalarView(base.value, !base.conjugated))
      case _: Vobla.ApplyConjugatedExpression if methodView.get.conjugated =>
        (base: ScalarView) => OpMonad(ScalarView(base.value, !base.conjugated))
      case _ => OpMonad(_: ScalarView)
    })(transformScalarExpression(e.base))
    case e: Vobla.TowOpExpression => !!(e match {
      case _: Vobla.PlusExpression => createPlusExpression
      case _: Vobla.MinusExpression => createMinusExpression
      case _: Vobla.MultExpression => createMultExpression
      case _: Vobla.DivExpression => createDivExpression
    })(transformScalarExpression(e.exp0), transformScalarExpression(e.exp1))
    case e => ice(e, "unexpected expression")
  }

  private val transformArrayExpression: Vobla.Expression => OpMonad[ArrayView] = _ match {
    case e: Vobla.Variable => transformArrayVariable(e)
    case e: Vobla.ArrayConstant => createArrayConstant(e)
    case e: Vobla.OffsetExpression => transformArrayOffset(e)
    case e: Vobla.OneOpExpression => !!(e match {
      case _: Vobla.DiagExpression => transformDiag
      case _: Vobla.AntiDiagExpression => transformAntiDiag
      case _: Vobla.TransposedExpression => transformTransposed
      case _: Vobla.ReversedExpression => transformReveresed
      case _: Vobla.ConjExpression => transformArrayConj
      case _: Vobla.ApplyConjugatedExpression if methodView.get.conjugated => transformArrayConj
      case _ => identity(_: ArrayView)
    })(transformArrayExpression(e.base))
    case e => ice(e, "Array expression expected")
  }

  private def swapArgs[T0, T1](f: (T0, T0) => T1) = (a: T0, b: T0) => f(b, a)

  private def transformObjectExpression(e: Vobla.Expression): OpMonad[ObjectView] = e match {
    case v: Vobla.Variable => varTable.get(v) match {
      case None => ice(v, "Missing interface")
      case Some(view: ObjectView) => view
      case v => ice(v, "Object expected")
    }
    case Vobla.ObjScalarMultExpression(exp0, exp1, t) =>
      !!(transformArrayScalarOp(createMultExpression))(transformScalarExpression(exp0), transformObjectExpression(exp1), t.base)
    case Vobla.DivExpression(exp0, exp1, t) =>
      !!(transformArrayScalarOp(swapArgs(createDivExpression)))(transformScalarExpression(exp1), transformObjectExpression(exp0), t.base)

    case op: Vobla.PlusExpression => transform2OpArrayExpression(createPlusExpression, op)
    case op: Vobla.MinusExpression => transform2OpArrayExpression(createMinusExpression, op)
    case Vobla.ConjExpression(base) => !!((i: ObjectView) => i.updateConjugated(!i.conjugated))(transformObjectExpression(base))
    case Vobla.ApplyConjugatedExpression(base) if methodView.get.conjugated =>
      !!((i: ObjectView) => i.updateConjugated(!i.conjugated))(transformObjectExpression(base))
    case Vobla.ConstExpression(base) => transformObjectExpression(base)
    case Vobla.ApplyConjugatedExpression(base) => transformObjectExpression(base)
    case Vobla.UMinusExpression(base, t) => !!(transform1OpArrayExpression(createUMinusExpression, true))(transformObjectExpression(base), t.base)
    case Vobla.ConstructorExpression(expType, args) =>
      val OpMonad(oldView_, ops0) = getSimpleView(methodView.get)
      val oldView = oldView_.asInstanceOf[SimpleInterfaceView]
      val argsMonad: OpMonad[List[ScalarView]] = args.map(transformScalarExpression)
      val varTable = oldView.varTable.filterKeys(expType.storage.members.locals.toSet) ++ (expType.members, argsMonad.value).zipped
      OpMonad(VirtualInterfaceView(expType.dim, oldView.storages, varTable, expType, oldView.specType,
          oldView.conjugated, oldView.restrict), ops0 ++ argsMonad.ops)
    case Vobla.ViewConstructorExpression(view, base, expType) =>
      val OpMonad(viewBase, ops) = !!(getSimpleView)(transformObjectExpression(base))
      OpMonad(VirtualInterfaceView(view.dim, viewBase.storages, Map(view.base -> viewBase), view, expType.base,
          viewBase.conjugated, viewBase.restrict), ops)
    case e: Vobla.BaseMethodCallExpression =>
      val obj = (e, methodView) match {
        case (e: Vobla.MethodCallExpression, _) => transformObjectExpression(e.base)
        case (_, Some(obj)) => OpMonad(obj)
        case e => ice(e, "Invalid self method call")
      }
      !!(transformMethodCallExpression)(obj, e) match {
        case OpMonad(s: ObjectView, ops) => OpMonad(s, ops)
        case e => ice(e, "OpMonad[ObjectView] expected")
      }
    case e => transformArrayExpression(e)
  }

  private def createComposedScalarMethodCall(op: ScalarView => OpMonad[ScalarView])(obj: ObjectView, m: Vobla.FunctionDecl) =
    m -> ((args: List[ScalarView]) => !!((v: Any) => op(v.asInstanceOf[ScalarView]))(transformMethodCall(obj, m, args)))

  private def createComposedRangeMethodCall(op: List[View] => OpMonad[List[View]])(obj: ObjectView, m: Vobla.FunctionDecl) = {
    val f = (_: List[ScalarView]) => {
      val OpMonad(v0, ops0) = transformMethodCall(obj, m, Nil)
      val oldLoopMaker = v0.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation]
      OpMonad((oldYieldOp: YieldCallback, parallel: Boolean) => {
        val newYieldOp = (iters: List[View]) => !!!(oldYieldOp)(op(iters))
        oldLoopMaker(newYieldOp, parallel)
      }, ops0)
    }
    (m -> f)
  }

  private val transform1OpArrayExpression = (op: ScalarView => OpMonad[ScalarView], sparseIter: Boolean) =>
    (obj: ObjectView, scalarType: Vobla.ScalarType) => {
    val sizes = List.tabulate(obj.dimension)(i => createComposedScalarMethodCall(identity)(obj, Vobla.BuiltInInterfaces.GetLen(i+1)))
    val rangeOp = (idxs: List[View]) => !!(idxs.init.:+(_: ScalarView))(op(idxs.last.asInstanceOf[ScalarView]))
    val iterate = createComposedRangeMethodCall(rangeOp)(obj, Vobla.BuiltInInterfaces.Iterate(obj.dimension))
    val sparse = if(sparseIter) createComposedRangeMethodCall(rangeOp)(obj, Vobla.BuiltInInterfaces.SparseIterate(obj.dimension)) else
      (Vobla.BuiltInInterfaces.SparseIterate(obj.dimension) -> iterate._2)
    val access = createComposedScalarMethodCall(op)(obj, Vobla.BuiltInInterfaces.Access(obj.dimension))
    ComposedInterfaceView(obj.dimension, false, (access :: sparse :: iterate :: sizes).toMap, transformScalarType(scalarType))
  }

  private def transform2OpArrayExpression(op: (ScalarView, ScalarView) => OpMonad[ScalarView], exp: Vobla.TwoOpVecOp): OpMonad[ObjectView] =
    (exp.exp0.expType, exp.exp1.expType) match {
    case (_: Vobla.ScalarType, _) => !!(transform1OpArrayExpression(!!(op)(transformScalarExpression(exp.exp0), _: ScalarView), false))(
      transformObjectExpression(exp.exp1), exp.expType.base)
    case (_, _: Vobla.ScalarType) => !!(transform1OpArrayExpression(!!(op)(_: ScalarView, transformScalarExpression(exp.exp1)), false))(
      transformObjectExpression(exp.exp0), exp.expType.base)
    case (_, _) => !!(transform2OpArrayExpression(op, exp.left, exp.expType.base))(transformObjectExpression(exp.exp0),
      transformObjectExpression(exp.exp1))
  }

  private def transform2OpArrayExpression(op: (ScalarView, ScalarView) => OpMonad[ScalarView], left: Boolean, scalarType: Vobla.ScalarType):
    (ObjectView, ObjectView) => OpMonad[ObjectView] = (e0, e1) => {
  val assumes = List.tabulate(e0.dimension)(i => !!!(createAssumeEq)(getSize(i+1)(e0), getSize(i+1)(e1)))
    val sizes = List.tabulate(e0.dimension)(i => createComposedScalarMethodCall(identity)(e0, Vobla.BuiltInInterfaces.GetLen(i+1)))
    val accessDecl = Vobla.BuiltInInterfaces.Access(e0.dimension)
    val iterateDecl = Vobla.BuiltInInterfaces.Iterate(e0.dimension)
    val iterate = iterateDecl -> ((_: List[ScalarView]) => if(left) {
      val OpMonad(v0, ops0) = transformMethodCall(e0, iterateDecl, Nil)
      val oldLoopMaker = v0.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation]
      OpMonad((oldYieldOp: YieldCallback, parallel: Boolean) => {
        val newYieldOp = (iters: List[View]) => {
          val v1 = transformMethodCall(e1, accessDecl, iters.init.map(_.asInstanceOf[ScalarView]))
          val value = !!((x: Any) => op(iters.last.asInstanceOf[ScalarView], x.asInstanceOf[ScalarView]))(v1)
          !!!((x: ScalarView) => oldYieldOp(iters.init :+ x))(value)
        }
        oldLoopMaker(newYieldOp, parallel)
      }, ops0)
    } else {
      val OpMonad(v1, ops0) = transformMethodCall(e1, iterateDecl, Nil)
      val oldLoopMaker = v1.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation]
      OpMonad((oldYieldOp: YieldCallback, parallel: Boolean) => {
        val newYieldOp = (iters: List[View]) => {
          val v0 = transformMethodCall(e0, accessDecl, iters.init.map(_.asInstanceOf[ScalarView]))
          val value = !!((x: Any) => op(x.asInstanceOf[ScalarView], iters.last.asInstanceOf[ScalarView]))(v0)
          !!!((x: ScalarView) => oldYieldOp(iters.init :+ x))(value)
        }
        oldLoopMaker(newYieldOp, parallel)
      }, ops0)
    })
    val sparse = Vobla.BuiltInInterfaces.SparseIterate(e0.dimension) -> iterate._2
    // Access method
    val access = accessDecl -> ((args: List[ScalarView]) => {
    val v0 = transformMethodCall(e0, accessDecl, args)
    val v1 = transformMethodCall(e1, accessDecl, args)
    !!((a: Any, b: Any) => op(a.asInstanceOf[ScalarView], b.asInstanceOf[ScalarView]))(v0, v1)
  })
    ComposedInterfaceView(e0.dimension, false, (access :: sparse :: iterate :: sizes).toMap, transformScalarType(scalarType))
  }

  private val transformArrayScalarOp = (op: (ScalarView, ScalarView) => OpMonad[ScalarView]) =>
    (exp0: ScalarView, obj: ObjectView, scalarType: Vobla.ScalarType) =>
      transform1OpArrayExpression(op(exp0, _:ScalarView), true)(obj, scalarType)

  private val transformArrayPlusExpression = (exp0: ArrayView, exp1: ArrayView) => {
    assert(exp0.dimension == exp1.dimension, (exp0, exp1), "Arrays must have the same dimension")
    val assumes = (exp0.sizes, exp1.sizes).zipped.map((s0, s1) =>
      new Pencil.AssumeOperation(Pencil.EqualExpression(s0, s1)))
    val apply = (offsets: List[Pencil.ScalarExpression]) =>
      !!(createPlusExpression)(applyIdx(exp0, offsets), applyIdx(exp1, offsets))
    OpMonad(createComposedArray(exp0, apply), assumes)
  }

  private val transformArrayMinusExpression = (exp0: ArrayView, exp1: ArrayView) => {
    assert(exp0.dimension == exp1.dimension, (exp0, exp1), "Arrays must have the same dimension")
    val assumes = (exp0.sizes, exp1.sizes).zipped.map((s0, s1) =>
      new Pencil.AssumeOperation(Pencil.EqualExpression(s0, s1)))
    val apply = (offsets: List[Pencil.ScalarExpression]) =>
      !!(createMinusExpression)(applyIdx(exp0, offsets), applyIdx(exp1, offsets))
    OpMonad(createComposedArray(exp0, apply), assumes)
  }

  private def createComposedArray(model: ArrayView, apply: List[Pencil.ScalarExpression] => OpMonad[ScalarView]) = {
    val offsets = List.fill(model.dimension)(IndexZero)
    val factors = List.tabulate(model.dimension, model.dimension)((i, j) => if (i == j) 1 else 0)
    ComposedArrayView(false, Vobla.ArrayPart.Full, model.sizes, offsets, factors, model.baseType, apply)
  }

  private val transformMethodCallExpression = (obj: View, e: Vobla.BaseMethodCallExpression) => obj match {
    case v: ArrayView => transformArrayMethodCall(v, e)
    case v: InterfaceView => transformInterfaceMethodCall(v, e)
  }


  private val transformMethodCall = (obj: ObjectView, m: Vobla.FunctionDecl, args: List[ScalarView]) => obj match {
    case v: ArrayView => transformArrayMethodCall(v, m, args)
    case v: InterfaceView => transformInterfaceMethodCall(v, m, args)
  }

  private def transformArrayMethodCall(view: ArrayView, m: Vobla.BaseMethodCallExpression): OpMonad[_] =
    !!(transformArrayMethodCall(_: ArrayView, _: Vobla.FunctionDecl, _: List[ScalarView]))(view,
        m.method, m.args.map(transformScalarExpression))

  private def transformArrayMethodCall(view: ArrayView, m: Vobla.FunctionDecl, args: List[ScalarView]): OpMonad[_] = (m, args) match {
    case (Vobla.BuiltInInterfaces.GetLen(i), List()) => ScalarView(view.sizes(i - 1))
    case (Vobla.BuiltInInterfaces.GetRange(dim), List()) if view.dimension == dim =>
      val (ranges, vars) = view.sizes.map{ size =>
        val rangeVar = createScalarVariable("i", Vobla.IndexType(true))
        (new Pencil.Range(rangeVar.value, IndexZero, createMinus(size, IndexOne), IndexOne), rangeVar)
      }.unzip
      (y: YieldCallback, p: Boolean) => ranges.foldLeft(y(vars))((op, range) => createForOperation(range, op, p))
    case (Vobla.BuiltInInterfaces.Iterate(dim), List()) if view.dimension == dim => transformArrayIterate(view)
    case (Vobla.BuiltInInterfaces.SparseIterate(dim), List()) if view.dimension == dim => transformArrayIterate(view)
    case (Vobla.BuiltInInterfaces.Re(dim), List()) if view.dimension == dim =>
      view.copy(arrayPart = Vobla.ArrayPart.combine(view.arrayPart, Vobla.ArrayPart.Re))
    case (Vobla.BuiltInInterfaces.Im(dim), List()) if view.dimension == dim =>
      view.copy(arrayPart = Vobla.ArrayPart.combine(view.arrayPart, Vobla.ArrayPart.Im))
    case (Vobla.BuiltInInterfaces.Access(dim), offsets) if view.dimension == dim =>
      !!(applyIdx)(view, offsets.map(_.value))
    case _ => ice(m, "Invalid method call on array")
  }

  private def transformArrayIterate(view: ArrayView) = {
   val (ranges, vars) = view.sizes.map{ size =>
      val rangeVar = createScalarVariable("i", Vobla.IndexType(true))
      (new Pencil.Range(rangeVar.value, IndexZero, createMinus(size, IndexOne), IndexOne), rangeVar)
    }.unzip
    val access = applyIdx(view, vars.map(v => v.value))
    (y: YieldCallback, p: Boolean) => {
      val op: Pencil.Operation = new Pencil.BlockOperation(access.ops :+ y(vars :+ access.value))
      ranges.foldLeft(op)((op, range) => createForOperation(range, op, p))
    }
  }

  // Transform a method call, but not for ranges
  private def transformInterfaceMethodCall(view: InterfaceView, m: Vobla.BaseMethodCallExpression): OpMonad[_] =
    !!(transformInterfaceMethodCall(_: InterfaceView, _: Vobla.FunctionDecl, _: List[ScalarView]))(view,
        m.method, m.args.map(transformScalarExpression))

  private def transformInterfaceMethodCall(view: InterfaceView, m: Vobla.FunctionDecl, args: List[ScalarView]) = view match {
    case v: SimpleInterfaceView => transformSimpleInterfaceMethodCall(v, m, args)
    case v: ComposedInterfaceView => v.methods(m)(args)
  }

  private def transformSimpleInterfaceMethodCall(view: SimpleInterfaceView, m: Vobla.FunctionDecl, args: List[ScalarView]): OpMonad[_] = {
    val method = view.voblaType.methods(m)
    // Create the arguments
    val (init, argVars) = (args, method.args).zipped.map{ (exp, arg) =>
      val argTmp = createScalarVariable("method_arg", arg.expType.asInstanceOf[Vobla.ScalarType].setAssignable(true))
      (!!!(createAssignment)(argTmp, exp), (arg, argTmp))
    }.unzip
    // Register variables
    val newVarTable = varTable.clone
    newVarTable ++= argVars
    newVarTable ++= view.varTable
    // Compile the body
    init ++: (method match {
      case method: Vobla.MethodImpl =>
        val transformer = new FunctionTransformer(getValueVersion(view.specType), structures, functionVersions,
            additionalFunctions, newVarTable, None, true, Some(view))
        transformer.transformMethod(method)
      case method: Vobla.RangeImpl => OpMonad((yieldOp: YieldCallback, parallel: Boolean) => {
        val transformer = new FunctionTransformer(getValueVersion(view.specType), structures, functionVersions,
            additionalFunctions, newVarTable, Some(yieldOp), parallel, Some(view))
        transformer.transformRangeMethod(method)
      })
    })
  }

  def transformMethod(method: Vobla.MethodImpl) = {
    method.locals.foreach(registerLocalVariable)
    val exp = method.returnOp.expression.get
    transformBody(method.body) +: (exp.expType match {
      case _: Vobla.ScalarType => transformScalarExpression(exp)
      case _ => transformObjectExpression(exp)
    })
  }

  def transformRangeMethod(method: Vobla.RangeImpl) = {
    method.locals.foreach(registerLocalVariable)
    transformBody(method.body)
  }

  private val transformVecMult = (exp0: ArrayView, exp1: ArrayView, t: Vobla.Type) => {
    val resType = t match {
      case t: Vobla.ScalarType => t
      case t => ice(t, "Invalid return type for dot product")
    }
    val (size, assume) = (exp0.sizes, exp1.sizes) match {
      case (List(s0), List(s1)) => (s0, new Pencil.AssumeOperation(Pencil.EqualExpression(s0, s1)))
      case e => ice(e, "Invalid array dimension")
    }
    val iter = Pencil.ScalarVariable(IndexType, "iter", None)
    val accum = createScalarVariable("acc", resType.setAssignable(true))

    val tmp = !!(createPlusExpression)(applyIdx(exp0, List(iter)), applyIdx(exp1, List(iter)))
    val body = !!!(createAssignment)(accum, !!(createPlusExpression)(accum, tmp))

    val range = new Pencil.Range(iter, IndexZero, createMinus(size, IndexOne), IndexOne)
    val loop = createForOperation(range, body, false)
    val init = createAssignment(accum, ScalarView(getScalarZero(RealType)))
    OpMonad(accum, List(assume, init, loop))
  }

  val transformComparison = (exp0: ScalarView, exp1: ScalarView, op: ComparisonOp.ComparisonOp) =>  createPencilBinaryExp(op match {
      case ComparisonOp.EQ => Pencil.EqualExpression
      case ComparisonOp.NEQ => Pencil.NEqualExpression
      case ComparisonOp.MORE => Pencil.GreaterExpression
      case ComparisonOp.LESS => Pencil.LessExpression
      case ComparisonOp.MOREEQ => Pencil.GreaterEqExpression
      case ComparisonOp.LESSEQ => Pencil.LessEqExpression
    })(exp0.value, exp1.value)

  private val transformNot = (exp: Pencil.ScalarExpression) => Pencil.NotExpression(exp)
  private val transformAnd = (exp0: Pencil.ScalarExpression, exp1: Pencil.ScalarExpression) => Pencil.AndExpression(exp0, exp1)
  private val transformOr = (exp0: Pencil.ScalarExpression, exp1: Pencil.ScalarExpression) => Pencil.OrExpression(exp0, exp1)

  def transformGuard(in: Vobla.Condition): OpMonad[Pencil.ScalarExpression] = in match {
    case Vobla.Comparison(op, e0, e1) =>
      !!(transformComparison)(transformScalarExpression(e0), transformScalarExpression(e1), op)
    case Vobla.LogicalNot(e) => !!(transformNot)(transformGuard(e))
    case Vobla.LogicalAnd(e0, e1) => !!(transformAnd)(transformGuard(e0), transformGuard(e1))
    case Vobla.LogicalOr(e0, e1) => !!(transformOr)(transformGuard(e0), transformGuard(e1))
  }

  private val transformIfOperation = (guard: Pencil.ScalarExpression, in: Vobla.IfOperation) => {
    val ebody = if(in.elseBody.isEmpty) None else Some(transformBody(in.elseBody))
    val body = transformBody(in.ifBody)
    new Pencil.IfOperation(guard, body, ebody)
  }

  /**
   * Convert VOBLA statement to PENCIL statement.
   * @param in - VOBLA statement.
   * @return - PENCIL statement.
   */
  private val transformOperation: Vobla.Operation => Pencil.Operation = _ match {
    case op: Vobla.MovOperation => transformOpAssignment(createAssignment)(op)
    case op: Vobla.MovPlusOperation => transformOpAssignment(createOpAssignment(createPlusExpression))(op)
    case op: Vobla.MovMinusOperation => transformOpAssignment(createOpAssignment(createMinusExpression))(op)
    case op: Vobla.MovMulOperation => transformOpAssignment(createOpAssignment(createMultExpression))(op)
    case op: Vobla.MovDivOperation => transformOpAssignment(createOpAssignment(createDivExpression))(op)
    case Vobla.ForOperation(iters, body, parallel) => transformIterations(parallel, body, iters)
    case op: Vobla.IfOperation => !!!(transformIfOperation)(transformGuard(op.condition), op)
    case ret: Vobla.ReturnOperation => transformReturnOperation(ret)
    case Vobla.WhileOperation(guard, body) => !!!(transformWhileOperation)(transformGuard(guard), transformBody(body))
    case Vobla.CallOperation(e) => !!!(transformCallOperation)(transformCallExpression(e))
    case op: Vobla.ArrayDeclOperation => transformArrayDeclOperation(op)
    case Vobla.AssumeOperation(voblaCond) => !!!(createAssume)(transformGuard(voblaCond))
    case Vobla.YieldOperation(args) => yieldOp match {
      case Some(f) => !!!(f)(args.map{ e => e.expType match {
        case _: Vobla.ScalarType => transformScalarExpression(e)
        case _ => transformObjectExpression(e)
      }})
      case None => ice(None, "Yield callback expected")
    }
    case op => ice(op, "unexpected operation")
  }

  private val createAssume = (exp: Pencil.ScalarExpression) => new Pencil.AssumeOperation(exp)

  private def transformArrayDeclOperation(op: Vobla.ArrayDeclOperation) = {
    val (baseType, sizes, ops, assignExp, initExp) = op match {
      case Vobla.ArrayDecl(_, baseType, voblaSizes) =>
        val sizes = voblaSizes.map(transformScalarExpression)
        val OpMonad(s, ops) = monadList2ListMonad(sizes.map(!!((s: ScalarView) => s.value)))
        (transformScalarType(baseType), s, ops, None, None)
      case Vobla.TypeofArrayDecl(_, e: Vobla.ArrayConstant, assign) =>
        val cst = transformArrayConstant(e)
        val (baseType, sizes) = decomposeArrayType(cst.expType)
        (baseType, sizes, List(), None, if (assign) Some(cst) else None)
      case Vobla.TypeofArrayDecl(_, e, assign) =>
        val exp = transformArrayExpression(e)
        (exp.value.baseType, exp.value.sizes, exp.ops, if (assign) Some(exp) else None, None)
    }
    val (view, v) = createArrayVariable(baseType, sizes, true, initExp)
    varTable += ((op.variable, view))
    val assignment = assignExp match {
      case None => None
      case Some(e) => Some(!!!(copyArray)(view, e))
    }
     make(make(ops.map(op => Some(op)): _*), Some(new Pencil.ArrayDeclOperation(v)), assignment).get
  }

  private val transformWhileOperation = (guard: Pencil.ScalarExpression, body: Pencil.BlockOperation) => new Pencil.WhileOperation(guard, body)

  private val transformCallOperation = (expr: ScalarView) => expr.value match {
    case call: Pencil.CallExpression => new Pencil.CallOperation(call)
    case _ => ice(expr, "expression cannot be used as a standalone call")
  }

  private def transformReturnOperation(returnOp: Vobla.ReturnOperation) = returnOp match {
    case Vobla.ReturnOperation(None, _) => new Pencil.ReturnOperation(None)
    case Vobla.ReturnOperation(Some(e), t: Vobla.ScalarType) => !!!(createReturnWithValue(t))(transformScalarExpression(e))
    case Vobla.ReturnOperation(_, t) => ice(t, "scalar type expected")
  }

  private def createReturnWithValue(t: Vobla.ScalarType) = (exp: ScalarView) => {
    val out = (exp.conjugated && !exp.value.expType.isNumeric) || (exp.value.expType.isNumeric && isComplex(transformScalarType(t))) match {
      case false => OpMonad(exp.value)
      case true =>
        val v = Pencil.ScalarVariable(exp.value.expType, "var", None)
        OpMonad(v, createAssignment(new LScalarView(v), exp))
    }
    !!!((out: Pencil.ScalarExpression) => new Pencil.ReturnOperation(Some(convertScalar(out, transformScalarType(t)))))(out)
  }

  private def getScalarLvalue(in: Vobla.Expression) = transformScalarExpression(in) match {
    case OpMonad(v: LScalarView, ops) => OpMonad(v, ops)
    case v => ice(v.value, "lvalue expected")
  }

  /**
   * Create new PENCIL for loop, which executes given operation.
   * @param op - PENCIL operation to be placed in the loop body.
   * @param upper - Upper bound of loop iteration range.
   * @param iter - Loop iterator.
   *
   * Lower iteration bound is zero.
   *
   * @return PENCIL block, containing created loop.
   */
  // TODO : use createFor instead
  private def translateToLoop(body: Pencil.Operation, upper: Pencil.ScalarExpression, iter: Pencil.ScalarVariable) = {
    val range = new Pencil.Range(iter, IndexZero, upper, IndexOne)
    val loop = new Pencil.ForOperation(List(new Pencil.IndependentLoop(None)), range, new Pencil.BlockOperation(List(body)))
    new Pencil.BlockOperation(List(loop))
  }

  /**
   * For a given non-scalar assignment operation generate a loop nest, which performs the assignment.
   *
   * @param lvalue - initial LHS operand of the assignment.
   * @param rvalue - initial RHS operand of the assignment.
   * @param iters - Set of iterators for non-scalar to scalar conversion
   * @param _type - Current type of the expression.
   * @return PENCIL block, which performs the assignment.
   *
   * The function creates loop nest according to the _type argument. When the innermost loop is created,
   * both lvalue and rvalue are converted to scalars by applying generated indices.
   *
   */
  private def scalarize(lvalue: ArrayView, rvalue: ArrayView, iters: List[Pencil.ScalarVariable],
    sizes: List[Pencil.ScalarExpression]): Pencil.Operation = sizes match {
    case h :: q =>
      val iter = Pencil.ScalarVariable(IndexType, "iter", None)
      val upper = createMinus(h, IndexOne)
      translateToLoop(scalarize(lvalue, rvalue, iter :: iters, q), upper, iter)
    case List() =>
      val lval = applyIdx(lvalue, iters) match {
        case OpMonad(v: LScalarView, ops) => OpMonad(v, ops)
        case v => ice(v, "Left value expected")
      }
      val rval = applyIdx(rvalue, iters)
      !!!(createAssignment)(lval, rval)
  }

  /**
   * Convert VOBLA non-scalar assignment operation to a PENCIL operation.
   *
   * Non-scalar assignments can not be directly transformed to PENCIL,
   * since PENCIL doesn't support array assignment. Such operations are transformed
   * by wrapping the assignment operations in a loop nest. For example
   * consider the following VOBLA code (A, B and C are 2D N x N arrays):
   *
   * A = B + C;
   *
   * Corresponding PENCIL code will look as follows:
   *
   * for (i1 = 0; i1 <= N - 1; i1 += 1)
   *  for (i2 = 0; i2 <= N - 1; i2 += 1)
   *    A[i1][i2] = B[i1[i2] + C[i1][i2];
   *
   * @param lvalue - Left side operand of the assignment.
   * @param rvalue - Right side operand of the assignment.
   * @return PENCIL operation (for loop), performing the assignment.
   */
  private def transformArrayAssignment(op: (LScalarView, ScalarView) => Pencil.Operation, left: Boolean, sparse: Boolean) =
    (lvalue: ObjectView, rvalue: ObjectView) => {
    assert(lvalue.dimension == rvalue.dimension, (lvalue, rvalue), "Arrays must have the same dimension")
    val iterate = (if(sparse) Vobla.BuiltInInterfaces.SparseIterate else Vobla.BuiltInInterfaces.Iterate)(lvalue.dimension)
    val access = Vobla.BuiltInInterfaces.Access(lvalue.dimension)
    val assumes = List.tabulate(lvalue.dimension)(i => !!!(createAssumeEq)(getSize(i+1)(lvalue), getSize(i+1)(rvalue)))
    if(left) {
      val OpMonad(v0, ops0) = !!(transformMethodCall)(lvalue, iterate, Nil)
      val yieldOp = (iters: List[View]) => {
        val OpMonad(v1, ops1) = !!(transformMethodCall)(rvalue, access,  iters.init.map(_.asInstanceOf[ScalarView]))
        !!!(op)(OpMonad(iters.last.asInstanceOf[LScalarView], ops0), OpMonad(v1.asInstanceOf[ScalarView], ops1))
      }
      v0.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation](yieldOp, true)
    } else {
      val OpMonad(v1, ops1) = !!(transformMethodCall)(rvalue, iterate, Nil)
      val yieldOp = (iters: List[View]) => {
        val OpMonad(v0, ops0) = !!(transformMethodCall)(lvalue, access, iters.init.map(_.asInstanceOf[ScalarView]))
        !!!(op)(OpMonad(v0.asInstanceOf[LScalarView], ops0), OpMonad(iters.last.asInstanceOf[ScalarView], ops1))
      }
      v1.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation](yieldOp, true)
    }
  }

  private val copyArray = (lvalue: ArrayView, rvalue: ArrayView) => scalarize(lvalue, rvalue, List(), lvalue.asInstanceOf[ArrayView].sizes)

  private val createAssumeEq = (l: Pencil.ScalarExpression, r: Pencil.ScalarExpression) =>
    new Pencil.AssumeOperation(Pencil.EqualExpression(l, r))

  private def getSize(i: Int) = (_: ObjectView) match {
    case v: ArrayView => OpMonad(v.sizes(i-1))
    case v: InterfaceView =>
      val OpMonad(s, ops) = transformInterfaceMethodCall(v, Vobla.BuiltInInterfaces.GetLen(i), Nil)
      s match {
        case s: ScalarView => OpMonad(s.value, ops)
        case s => ice(s, "Scalar expression expected")
      }
  }

  private val createOpAssignment = (op: (ScalarView, ScalarView) => OpMonad[ScalarView]) =>
    (lv: LScalarView, rv: ScalarView) => !!!(createAssignment)(lv, op(lv, rv))

  private val transformOpAssignment = (op: (LScalarView, ScalarView) => Pencil.Operation) => (in: Vobla.MovOp) => {
    (in.lValue.expType, in.rValue.expType) match {
      case (_: Vobla.ScalarType, _: Vobla.ScalarType) => !!!(op)(getScalarLvalue(in.lValue), transformScalarExpression(in.rValue))
      case (_, _: Vobla.ScalarType) =>  !!!(transformArrayScalarAssign(op, in.sparseIterate))(
          transformObjectExpression(in.lValue), transformScalarExpression(in.rValue))
      case (_, _) => !!!(transformArrayAssignment(op, in.leftIterate, in.sparseIterate))(
        transformObjectExpression(in.lValue), transformObjectExpression(in.rValue))
    }
  }

  private val transformArrayScalarAssign = (op: (LScalarView, ScalarView) => Pencil.Operation, sparse: Boolean) =>
    (lvalue: ObjectView, rvalue: ScalarView) => {
    val method = if(sparse) Vobla.BuiltInInterfaces.SparseIterate else Vobla.BuiltInInterfaces.Iterate
    val OpMonad(v0, ops0) = !!(transformMethodCall)(lvalue, method(lvalue.dimension), Nil)
      val yieldOp = (iters: List[View]) => !!!(op)(OpMonad(iters.last.asInstanceOf[LScalarView], ops0), rvalue)
      v0.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation](yieldOp, true)
  }


  private val createRangeFromSlice = (low: ScalarView, up: ScalarView, step: Int) => {
    val rangeVar = createScalarVariable("i", Vobla.IndexType(true))
    if(step > 0) (new Pencil.Range(rangeVar.value, low.value, up.value, Pencil.IntegerConstant(IndexType, step)), rangeVar)
    else (new Pencil.Range(rangeVar.value, low.value, up.value, Pencil.IntegerConstant(IndexType, -step)),
        ScalarView(createMinus(up.value, rangeVar.value)))
  }

  private def createYieldOp(op: () => Pencil.Operation, variables: List[Option[Vobla.Variable]])(exps: List[View]) = {
    assert(variables.size == exps.size, variables, "Invalid variable binding for yield")
    (variables, exps).zipped.foreach{
      case (Some(i), v) => varTable += ((i, v))
      case (None, _) =>
    }
    op()
  }

  private val transformRange = (e: Vobla.Expression, parallel: Boolean, yieldOp: YieldCallback) => e match {
    case Vobla.SliceRange(low, up, step) =>
      val lowExp = transformScalarExpression(low)
      val upExp = transformScalarExpression(up)
      val OpMonad((range, variable), ops) = !!(createRangeFromSlice)(lowExp, upExp, step)
      !!!(createForOperation)(OpMonad(range, ops), yieldOp(List(variable)), parallel)
    case e: Vobla.BaseMethodCallExpression =>
      val obj = (e, methodView) match {
        case (e: Vobla.MethodCallExpression, _) => transformObjectExpression(e.base)
        case (_, Some(obj)) => OpMonad(obj)
        case e => ice(e, "Invalid self method call")
      }
      val call = !!(transformMethodCallExpression)(obj, e)
      !!!((f: Any) => f.asInstanceOf[(YieldCallback, Boolean) => Pencil.Operation](yieldOp, parallel))(call)
    case e => ice(e, "Range expression expected")
  }

  private def transformIterations(parallel: Boolean, voblaBody: List[Vobla.Operation], iters: List[Vobla.Iteration]):
    Pencil.Operation = iters match {
    case Nil => transformBody(voblaBody)
    case Vobla.Iteration(variables, ranges) :: tail =>
      val yieldOp = createYieldOp(() => transformIterations(parallel, voblaBody, tail), variables)_
      transformRange(ranges, parallel && isParallel, yieldOp)
  }

  private val createForOperation = (range: Pencil.Range, body: Pencil.Operation, parallel: Boolean) => {
    val options = if (parallel) List(new Pencil.IndependentLoop(None)) else List()
    new Pencil.ForOperation(options, range, new Pencil.BlockOperation(List(body)))
  }

  def transformBody(in: List[Vobla.Operation]) = new Pencil.BlockOperation(in.map(lop => transformOperation(lop)))

  /**
   * Register all variables, declared in VOBLA program as PENCIL variables.
   * @param in - Table containing variables, declared in VOBLA program.
   */
  private def registerArgs(args: List[Vobla.Variable], version: Vobla.FunctionVersion) = {
    (args, version.args).zipped.flatMap((variable, version) => {
      val view = (variable.expType, version) match {
        case (vt: Vobla.ScalarType, Vobla.ScalarVersion(conjugated)) => createScalarVariable(variable.name, vt, conjugated)
        case (Vobla.InterfaceType(_, specType), version: Vobla.InterfaceVersion) => genInterArgView(version, specType)
        case (Vobla.ArrayType(base, _), v: Vobla.ArrayVersion) => genArrayArgument(base, v)
        case (Vobla.InterfaceType(_, specType), v: Vobla.ArrayVersion) => genArrayArgument(specType, v)
        case err => ice(err, "Bad argument/version pair")
      }
      varTable += ((variable, view))
      view.view :: view.storages
    })
  }

  private def genInterArgView(version: Vobla.InterfaceVersion, specType: Vobla.ScalarType): ConcreteInterfaceView = version.t match {
    case t: Vobla.TypeVersion => structures.getType(t.base, getValueVersion(specType), version.conjugated, version.restrict, specType)
    case Vobla.ViewVersion(base, subType: Vobla.MethodImplementerVersion) =>
      val subView = genInterArgView(version.copy(t = subType), specType)
      subView.copy(rawVarTable = Map(base.base -> subView), voblaType = base)
    case Vobla.ViewVersion(base, subType: Vobla.ArrayVersion) =>
      val subView = genArrayArgument(specType, subType)
      ConcreteInterfaceView(version.t.base.dim, subView.view, subView.storages, Map(base.base -> subView),
          base, specType, version.conjugated, version.restrict)
  }

  private def genArrayArgument(base: Vobla.ScalarType, version: Vobla.ArrayVersion) = {
    val dim = version.factors.size
    val baseDim = version.factors(0).size
    val viewVar = Pencil.ScalarVariable(structures.getArrayViewStruct(baseDim, dim), "view", None)
    val (baseSizes, rawSizes, offsets) = structures.getArrayViewMembers(viewVar, baseDim, dim)
    val sizes = if (version.reverseSizes) rawSizes.reverse else rawSizes
    val arrayType = genArrayType(transformScalarType(base), baseSizes)
    val arrayVar = Pencil.ArrayVariable(arrayType, "array", version.restrict, None)
    new ConcreteArrayView(viewVar, version.conjugated, version.restrict, version.arrayPart,
        arrayVar, sizes, baseSizes, offsets, version.factors)
  }

  private def createScalarVariable(name: String, t: Vobla.ScalarType, conjugated: Boolean = false) =
    ConcreteScalarView(Pencil.ScalarVariable(transformScalarType(t), name, None), conjugated)

  private def registerLocalVariable(v: Vobla.Variable) = v.expType match {
      case t: Vobla.ScalarType => varTable += ((v, createScalarVariable(v.name, t)))
      case _ =>
    }

  def transformFunctionDeclaration(name: String, version: Vobla.FunctionVersion, local: Boolean) = {
    version.function.vars.strictSubVariables.foreach(registerLocalVariable)
    val args = registerArgs(version.function.vars.locals, version)
    val returnType = version.function.decl.returnType match {
      case Vobla.VoidType() => Pencil.NopType
      case t: Vobla.ScalarType => transformScalarType(t)
      case t => ice(t, "Invalid return type")
    }
    new Pencil.Function(name, args, None, returnType, None, false, local, false)
  }
}
