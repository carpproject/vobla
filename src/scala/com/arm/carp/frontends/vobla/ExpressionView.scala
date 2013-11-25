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

import com.arm.carp.{ pencil => Pencil }
import com.arm.carp.{ vobla => Vobla }

private trait View {
  val conjugated: Boolean

  def updateConjugated(conjugated: Boolean): View
}

private trait ObjectView extends View {
  def dimension: Int
  override def updateConjugated(conjugated: Boolean): ObjectView
}

// A view which is not a composition of views by an operator
private trait SimpleView extends View {
  val storages: List[Pencil.ArrayVariable]
  val restrict: Boolean
  val version: Vobla.ArgVersion
}

// Represent a view stored in a structure
private trait ConcreteView extends SimpleView {
  val view: Pencil.ScalarVariable
}

private trait ScalarView extends SimpleView {
  val value: Pencil.ScalarExpression
  override val restrict = false
  override val storages = List.empty
  override val version = Vobla.ScalarVersion(conjugated)
}

private object ScalarView {
  def apply(value: Pencil.ScalarExpression, conjugated: Boolean = false) = value match {
    case v: Pencil.ScalarExpression with Pencil.LValue => new LScalarView(v, conjugated)
    case v => RScalarView(v, conjugated)
  }
}

private case class ConcreteScalarView(override val value: Pencil.ScalarVariable, override val conjugated: Boolean = false)
  extends LScalarView(value, conjugated) with ConcreteView {
  override val view = value
  override def updateConjugated(conjugated: Boolean) = copy(conjugated = conjugated)
}

private case class RScalarView(value: Pencil.ScalarExpression, conjugated: Boolean = false) extends ScalarView {
  override def updateConjugated(conjugated: Boolean) = copy(conjugated = conjugated)
}

private class LScalarView(
    override val value: Pencil.ScalarExpression with Pencil.LValue,
    override val conjugated: Boolean = false) extends ScalarView {
  override def updateConjugated(conjugated: Boolean) = new LScalarView(value, conjugated)
}

private trait ArrayView extends ObjectView {
  val arrayPart: Vobla.ArrayPart.ArrayPart
  val sizes: List[Pencil.ScalarExpression]
  val offsets: List[Pencil.ScalarExpression]
  val factors: List[List[Int]]
  val baseType: Pencil.ScalarType

  override def dimension = sizes.size
  override def updateConjugated(conjugated: Boolean) = copy(conjugated = conjugated)

  def copy(
    conjugated: Boolean = this.conjugated,
    restrict: Boolean = false,
    arrayPart: Vobla.ArrayPart.ArrayPart = this.arrayPart,
    sizes: List[Pencil.ScalarExpression] = this.sizes,
    offsets: List[Pencil.ScalarExpression] = this.offsets,
    factors: List[List[Int]] = this.factors): ArrayView
}

private case class ComposedArrayView(
  override val conjugated: Boolean,
  override val arrayPart: Vobla.ArrayPart.ArrayPart,
  override val sizes: List[Pencil.ScalarExpression],
  override val offsets: List[Pencil.ScalarExpression],
  override val factors: List[List[Int]],
  override val baseType: Pencil.ScalarType,
  applyOffsets: List[Pencil.ScalarExpression] => OpMonad[ScalarView])
    extends ArrayView {

  def apply() = applyOffsets(offsets)

  override def copy(
    conjugated: Boolean = this.conjugated,
    restrict: Boolean = false,
    arrayPart: Vobla.ArrayPart.ArrayPart = this.arrayPart,
    sizes: List[Pencil.ScalarExpression] = this.sizes,
    offsets: List[Pencil.ScalarExpression] = this.offsets,
    factors: List[List[Int]] = this.factors) =
      new ComposedArrayView(conjugated, arrayPart, sizes, offsets, factors, baseType, applyOffsets)
}

private class SimpleArrayView(
  override val conjugated: Boolean,
  override val restrict: Boolean,
  override val arrayPart: Vobla.ArrayPart.ArrayPart,
  val storage: Pencil.ArrayVariable,
  override val sizes: List[Pencil.ScalarExpression],
  val baseSizes: List[Pencil.ScalarExpression],
  override val offsets: List[Pencil.ScalarExpression],
  override val factors: List[List[Int]])
    extends SimpleView with ArrayView {

  override val storages = List(storage)
  override val baseType = getBaseType(storage.expType)
  override val version = Vobla.ArrayVersion(conjugated, false, arrayPart, factors)

  private def getBaseType: Pencil.ArrayType => Pencil.ScalarType = _.base match {
    case t: Pencil.ScalarType => t
    case t: Pencil.ArrayType => getBaseType(t)
  }

  override def copy(
    conjugated: Boolean = this.conjugated,
    restrict: Boolean = this.restrict,
    arrayPart: Vobla.ArrayPart.ArrayPart = this.arrayPart,
    sizes: List[Pencil.ScalarExpression] = this.sizes,
    offsets: List[Pencil.ScalarExpression] = this.offsets,
    factors: List[List[Int]] = this.factors) =
      new SimpleArrayView(conjugated, restrict, arrayPart, storage, sizes, baseSizes, offsets, factors)
}

private case class ConcreteArrayView(
  override val view: Pencil.ScalarVariable,
  override val conjugated: Boolean,
  override val restrict: Boolean,
  override val arrayPart: Vobla.ArrayPart.ArrayPart,
  override val storage: Pencil.ArrayVariable,
  override val sizes: List[Pencil.ScalarExpression],
  override val baseSizes: List[Pencil.ScalarExpression],
  override val offsets: List[Pencil.ScalarExpression],
  override val factors: List[List[Int]])
    extends SimpleArrayView(conjugated, restrict, arrayPart, storage, sizes, baseSizes, offsets, factors)
    with ConcreteView {

  override def copy(
    conjugated: Boolean = this.conjugated,
    restrict: Boolean = this.restrict,
    arrayPart: Vobla.ArrayPart.ArrayPart = this.arrayPart,
    sizes: List[Pencil.ScalarExpression] = this.sizes,
    offsets: List[Pencil.ScalarExpression] = this.offsets,
    factors: List[List[Int]] = this.factors) = {
    if(storage == this.storage && sizes == this.sizes && offsets == this.offsets && factors == this.factors)
      new ConcreteArrayView(view, conjugated, restrict, arrayPart, storage, sizes, baseSizes, offsets, factors)
    else
      new SimpleArrayView(conjugated, restrict, arrayPart, storage, sizes, baseSizes, offsets, factors)
  }
}

private trait SimpleInterfaceView extends SimpleView with InterfaceView {
  lazy val version = Vobla.InterfaceVersion(typeVersion, conjugated, restrict)
  val voblaType: Vobla.MethodImplementer
  val varTable: Map[Vobla.Variable, View]
  val specType: Vobla.ScalarType

  private lazy val typeVersion: Vobla.MethodImplementerVersion = voblaType match {
    case t: Vobla.VoblaType => Vobla.TypeVersion(t)
    case t: Vobla.VoblaView => varTable(t.base) match {
      case i: SimpleInterfaceView => Vobla.ViewVersion(t, i.typeVersion)
      case a: SimpleArrayView => Vobla.ViewVersion(t, a.version)
      case e => throw new Error("Unexpected view: " + e)
    }
  }

  protected def updateConjBase(conj: Boolean) = voblaType match {
    case t: Vobla.VoblaView => varTable + (t.base -> varTable(t.base).updateConjugated(conj))
    case _ => varTable
  }
}

private trait InterfaceView extends ObjectView {
  val dim: Int
  def dimension = dim
}

private case class ConcreteInterfaceView(
    dim: Int,
    view: Pencil.ScalarVariable,
    storages: List[Pencil.ArrayVariable],
    rawVarTable: Map[Vobla.Variable, View],
    voblaType: Vobla.MethodImplementer,
    specType: Vobla.ScalarType,
    conjugated: Boolean,
    restrict: Boolean)
  extends ConcreteView with SimpleInterfaceView {
  override val varTable = rawVarTable + (voblaType.thisVar -> this)
  override def updateConjugated(conjugated: Boolean) =
    ConcreteInterfaceView(dimension, view, storages, updateConjBase(conjugated), voblaType, specType, conjugated, restrict)
}

private case class VirtualInterfaceView(
    dim: Int,
    storages: List[Pencil.ArrayVariable],
    rawVarTable: Map[Vobla.Variable, View],
    voblaType: Vobla.MethodImplementer,
    specType: Vobla.ScalarType,
    conjugated: Boolean,
    restrict: Boolean)
  extends SimpleInterfaceView {
  override val varTable = rawVarTable + (voblaType.thisVar -> this)
  override def updateConjugated(conjugated: Boolean) =
    VirtualInterfaceView(dimension, storages, updateConjBase(conjugated), voblaType, specType, conjugated, restrict)
}

private case class ComposedInterfaceView(
    dim: Int,
    conjugated: Boolean,
    methods: Map[Vobla.FunctionDecl, List[ScalarView] => OpMonad[_]],
    scalarType: Pencil.ScalarType)
    extends InterfaceView {
  override def updateConjugated(conjugated: Boolean) = copy(conjugated = conjugated)
}
