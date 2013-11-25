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
import com.arm.carp.frontends.vobla.VoblaImplicits._
import scala.collection.mutable.HashMap

private class StructTransformer(
  valueType: Vobla.ValueVersion.ValueVersion,
  struct: StructProvider,
  restrict: Boolean = true)
    extends ValueTransformer(valueType, struct) {

  private val positions = HashMap[Vobla.Variable, Int]()

  def transformStructType(
      s: List[(String, Pencil.ScalarType)],
      members: List[Vobla.Variable],
      name: String,
      sizes: Map[Vobla.Variable, List[Vobla.Expression]]): (Pencil.StructType, HashMap[Vobla.Variable, Int])  = {
    (new Pencil.StructType(s ::: transformStructType(members, sizes, s.size), true, name), positions)
  }

  def transformStruct(
      structureType: Pencil.StructType,
      innerPos: Map[Vobla.Variable, Int],
      outterPos: Map[Vobla.Variable, Int],
      members: List[Vobla.Variable],
      sizes: Map[Vobla.Variable, List[Vobla.Expression]],
      assignable: Boolean) = {
    // Create the structure
    val struct = Pencil.ScalarVariable(structureType, "view", None)
    // Register the structure members
    val innerStruct = Pencil.ScalarStructSubscription(struct, 0)
    outterPos.foreach(p => varTable.put(p._1, ScalarView(Pencil.ScalarStructSubscription(struct, p._2))))
    innerPos.foreach(p => varTable.put(p._1, ScalarView(Pencil.ScalarStructSubscription(innerStruct, p._2))))
    // Create the arrays separately
    val arrays = members.flatMap(transformArrayMember(_, sizes, assignable))
    // Return all created variable and the table
    (struct, arrays, varTable.toMap)
  }

  private def transformArrayMember(member: Vobla.Variable, sizes: Map[Vobla.Variable, List[Vobla.Expression]], assignable: Boolean) = {
    member.expType match {
      case t: Vobla.ArrayType =>
        val base = t.base.setAssignable(assignable && t.base.assignable)
        val (view, variable) = createArrayVariable(transformScalarType(base), sizes(member).map(transformArraySize), restrict)
        varTable += ((member, view))
        Some(variable)
      case _ => None
    }
  }

  private def transformStructType(members: List[Vobla.Variable], sizes: Map[Vobla.Variable, List[Vobla.Expression]], shift: Int):
    List[(String, Pencil.ScalarType)] = members match {
    case Nil => List.empty
    case head :: tail => head.expType match {
      case t: Vobla.ScalarType =>
        positions.put(head, positions.size + shift)
        (head.name, transformScalarType(t.setAssignable(true))) :: transformStructType(tail, sizes, shift)
      case _ => transformStructType(tail, sizes, shift)
    }
  }

  private val transformArraySize: Vobla.Expression => Pencil.ScalarExpression = _ match {
    case Vobla.ConstExpression(base) => transformArraySize(base)
    case v: Vobla.Variable => varTable(v).asInstanceOf[ScalarView].value
    case Vobla.IndexConstant(i) => new Pencil.IntegerConstant(IndexType, i)
    case Vobla.UMinusExpression(base, _) => new Pencil.UnaryMinusExpression(transformArraySize(base))
    case e: Vobla.TowOpExpression =>
      val (exp0, exp1) = (transformArraySize(e.exp0), transformArraySize(e.exp1))
      e match {
        case _: Vobla.PlusExpression => new Pencil.PlusExpression(exp0, exp1)
        case _: Vobla.MinusExpression => new Pencil.MinusExpression(exp0, exp1)
        case _: Vobla.MultExpression => new Pencil.MultExpression(exp0, exp1)
        case _: Vobla.DivExpression => new Pencil.DivExpression(exp0, exp1)
      }
    case e => ice(e, "unexpected expression")
  }
}
