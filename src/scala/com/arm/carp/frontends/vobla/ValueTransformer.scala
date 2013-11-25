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

import scala.collection.mutable.HashMap

import com.arm.carp.{ pencil => Pencil }
import com.arm.carp.{ vobla => Vobla}
import com.arm.carp.frontends.vobla.VoblaImplicits._

private class ValueTransformer(
  valueType: Vobla.ValueVersion.ValueVersion,
  protected val structures: StructProvider,
  protected val varTable: HashMap[Vobla.Variable, View] = HashMap[Vobla.Variable, View]())
  extends Transformer {

  protected val RealType = valueType match {
    case Vobla.ValueVersion.FLOAT => FloatType
    case Vobla.ValueVersion.DOUBLE => DoubleType
    case Vobla.ValueVersion.COMPLEX_FLOAT => FloatType
    case Vobla.ValueVersion.COMPLEX_DOUBLE => DoubleType
  }

  protected val ValueType = valueType match {
    case Vobla.ValueVersion.FLOAT => FloatType
    case Vobla.ValueVersion.DOUBLE => DoubleType
    case Vobla.ValueVersion.COMPLEX_FLOAT => structures.ComplexFloatType
    case Vobla.ValueVersion.COMPLEX_DOUBLE => structures.ComplexDoubleType
  }

  protected val ComplexType = valueType match {
    case Vobla.ValueVersion.FLOAT => structures.ComplexFloatType
    case Vobla.ValueVersion.DOUBLE => structures.ComplexDoubleType
    case Vobla.ValueVersion.COMPLEX_FLOAT => structures.ComplexFloatType
    case Vobla.ValueVersion.COMPLEX_DOUBLE => structures.ComplexDoubleType
  }

  protected def transformScalarType(t: Vobla.ScalarType) = t match {
    case Vobla.ValueType(assignable)=> ValueType.updateConst(!assignable)
    case Vobla.RealType(assignable) => RealType.updateConst(!assignable)
    case Vobla.IndexType(assignable) => IndexType.updateConst(!assignable)
    case Vobla.ComplexType(assignable) => ComplexType.updateConst(!assignable)
  }

  protected def transformIndexCst(i: Int) = Pencil.IntegerConstant(IndexType, i)

  protected[vobla] def createArrayVariable(baseType: Pencil.ScalarType, sizes: List[Pencil.ScalarExpression], restrict: Boolean = true,
    init: Option[Pencil.ArrayConstant] = None) = {
    val t = genArrayType(baseType, sizes)
    val v = Pencil.ArrayVariable(t, "array_var", restrict, init)
    val factors = List.tabulate(sizes.size, sizes.size)((i, j) => if (i == j) 1 else 0)
    (new SimpleArrayView(false, true, Vobla.ArrayPart.Full, v, sizes, sizes, List.fill(sizes.size)(IndexZero), factors), v)
  }

  protected def genArrayType(baseType: Pencil.ScalarType, sizes: List[Pencil.ScalarExpression]) =
    sizes.init.foldRight(Pencil.ArrayType(baseType, sizes.last))((s, base) => Pencil.ArrayType(base, s))
}
