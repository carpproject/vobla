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

package com.arm.carp.vobla;

import scala.collection.mutable.HashMap
import collection.JavaConverters._
import com.arm.carp.vobla.ComparisonOp.ComparisonOp
import com.arm.carp.frontends.vobla.Memoize

trait Type {
  val assignable = false
  val > : Type => Boolean
  def setAssignable(b :Boolean): Type = this
  val base: ScalarType
  def specialize(t: ScalarType): Type = this
  val dim: Int
}

object Type {
  def calculateType: (ScalarType, ScalarType) => ScalarType = (_, _) match {
    case (_, ComplexType(_)) | (ComplexType(_), _) => ComplexType()
    case (_, ValueType(_)) | (ValueType(_), _) => ValueType()
    case (_, RealType(_)) | (RealType(_), _) => RealType()
    case _ => IndexType()
  }
}

trait ScalarType extends Type {
  override def setAssignable(b :Boolean) = this
  def setAssignableScalar(b: Boolean) = setAssignable(b).asInstanceOf[ScalarType]
  override val base = this
  override def specialize(t: ScalarType) = this.setAssignable(t.assignable && assignable)
  override val dim = 0
}

case class VoidType() extends Type {
  override val > = equals(_)
  override val base: ScalarType = IndexType()
  override val dim = -1
}

case class IndexType(override val assignable: Boolean = false) extends ScalarType {
  override def setAssignable(b :Boolean) = IndexType(b)
  override val > = (_: Type) match {
    case t: ScalarType if !t.assignable => true
    case IndexType(true) => assignable
    case _ => false
  }
}

case class ValueType(override val assignable: Boolean = false) extends ScalarType {
  override def setAssignable(b :Boolean) = ValueType(b)
  override val > = (_: Type) match {
    case ComplexType(false) | ValueType(false) => true
    case ValueType(true) => assignable
    case _ => false
  }
  override def specialize(t: ScalarType) = t.setAssignable(t.assignable && assignable)
}

case class RealType(override val assignable: Boolean = false) extends ScalarType {
  override def setAssignable(b :Boolean) = RealType(b)
  override val > = (_: Type) match {
    case ComplexType(false) | ValueType(false) | RealType(false) => true
    case RealType(true) => assignable
    case _ => false
  }
}

case class ComplexType(override val assignable: Boolean = false) extends ScalarType {
  override def setAssignable(b :Boolean) = ComplexType(b)
  override val > = (_: Type) match {
    case ComplexType(otherAssignable) => !otherAssignable || assignable
    case _ => false
  }
}

case class ArrayType(override val base: ScalarType, dim: Int) extends Type {
  override val > = (_: Type) match {
    case ArrayType(b, d) => d == dim && base > b
    case i: InterfaceType if InterfaceType(BuiltInInterfaces.array(dim), base) > i => true
    case _ => false
  }
  override val assignable = base.assignable
  override def setAssignable(assignable: Boolean) = ArrayType(base.setAssignable(assignable), dim)
  override def specialize(t: ScalarType) = ArrayType(base.specialize(t), dim)
}

case class InterfaceType(inter: Interface, override val base: ScalarType) extends Type {
  override val > = (_: Type) match {
    case InterfaceType(otherInter, otherBase) =>
    inter.subInterfaces.get(otherInter).exists{
      b => isCompatibleBase(b.specialize(base), otherBase)
    } || inter == otherInter && isCompatibleBase(base, otherBase)
    case _ => false
  }
  override def specialize(t: ScalarType) = InterfaceType(inter, base.specialize(t))
  override val assignable = base.assignable
  override def setAssignable(assignable: Boolean) = InterfaceType(inter, base.setAssignable(assignable))
  override val dim = inter.dim

  def subInterfaceBase(i: Interface) = inter.allInterfaces(i).specialize(base)

  private def isCompatibleBase(base: ScalarType, otherBase: ScalarType) =
    otherBase.setAssignable(base.assignable) == base || (!base.assignable && otherBase.assignable)
}

case class RangeType(subTypes: List[Type]) extends Type {
  override val > = (_: Type) match {
    case RangeType(ts) if ts.size == subTypes.size => (subTypes, ts).zipped.forall(_>_)
    case _ => false
  }
  val dimension = subTypes.size
  override def specialize(t: ScalarType) = RangeType(subTypes.map(_.specialize(t)))
  override val base: ScalarType = IndexType()
  override val dim = subTypes.size
}
