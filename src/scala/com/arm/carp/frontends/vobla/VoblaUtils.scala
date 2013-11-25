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

private object VoblaImplicits {
  implicit def ScalarVariable2ScalarVariableRef(value: Pencil.ScalarVariable) = new Pencil.ScalarVariableRef(value.base)
  implicit def ScalarVariable2ScalarVariableDef(value: Pencil.ScalarVariable) = value.base
  implicit def ScalarVariableRef2ScalarVariable(value: Pencil.ScalarVariableRef) = Pencil.ScalarVariable(value.variable)
  implicit def ScalarVariableDef2ScalarVariable(value: Pencil.ScalarVariableDef) = Pencil.ScalarVariable(value)
  implicit def ArrayVariable2ArrayVariableRef(value: Pencil.ArrayVariable) = new Pencil.ArrayVariableRef(value.base)
  implicit def ArrayVariable2ArrayVariableDef(value: Pencil.ArrayVariable) = value.base
  implicit def ArrayVariableRef2ArrayVariable(value: Pencil.ArrayVariableRef) = Pencil.ArrayVariable(value.variable)
  implicit def ArrayVariableDef2ArrayVariable(value: Pencil.ArrayVariableDef) = Pencil.ArrayVariable(value)
  implicit def VariableRef2Variable(value: Pencil.VariableRef) = value.variable
  implicit def VariableRefList2VariableList(value: List[Pencil.VariableRef]) = value.map(VariableRef2Variable)

  implicit def fromScalarVariable(obj: Pencil.ScalarVariable.type) = ScalarVariableFactory
  implicit def fromArrayVariable(obj: Pencil.ArrayVariable.type) = ArrayVariableFactory
}

private object ScalarVariableFactory {
  def apply(expType: Pencil.ScalarType, name: String, init: Option[Pencil.ScalarExpression]) =
    Pencil.ScalarVariable(Pencil.ScalarVariableDef(expType, name, init))
}

private object ArrayVariableFactory {
  def apply(expType: Pencil.ArrayType, name: String, restrict: Boolean, init: Option[Pencil.ArrayConstant]) =
    Pencil.ArrayVariable(Pencil.ArrayVariableDef(expType, name, restrict, init))
}

// Monad to handle additional operations
private case class OpMonad[+T](value: T, ops: List[Pencil.Operation] = List()) {
  def ++:(ops2: List[Pencil.Operation]) = OpMonad(value, ops2 ::: ops)
  def +:(ops2: Pencil.Operation) = OpMonad(value, ops2 :: ops)
}

private object OpMonad {
  implicit def opMonad[T](value: T) = OpMonad(value)

  implicit def monadList2ListMonad[T](l: List[OpMonad[T]]) = {
    val (vals, ops) = l.map(m => (m.value, m.ops)).unzip
    OpMonad(vals, ops.flatten)
  }

  def apply[T](value: T, op: Pencil.Operation): OpMonad[T] = OpMonad(value, List(op))

  // Binds
  def !![A, B](f: A => OpMonad[B]): OpMonad[A] => OpMonad[B] = a => {
    val b = f(a.value)
    OpMonad(b.value, a.ops ::: b.ops)
  }

  def !![A, B, C](f: (A, B) => OpMonad[C]): (OpMonad[A], OpMonad[B]) => OpMonad[C] = (a, b) => {
    val c = f(a.value, b.value)
    OpMonad(c.value, a.ops ::: b.ops ::: c.ops)
  }

  def !![A, B, C, D](f: (A, B, C) => OpMonad[D]): (OpMonad[A], OpMonad[B], OpMonad[C]) => OpMonad[D] = (a, b, c) => {
    val d = f(a.value, b.value, c.value)
    OpMonad(d.value, a.ops ::: b.ops ::: c.ops ::: d.ops)
  }

  def !![A, B, C, D, E](f: (A, B, C, D) => OpMonad[E]): (OpMonad[A], OpMonad[B], OpMonad[C], OpMonad[D]) => OpMonad[E] = (a, b, c, d) => {
    val e = f(a.value, b.value, c.value, d.value)
    OpMonad(e.value, a.ops ::: b.ops ::: c.ops ::: d.ops ::: e.ops)
  }

  // Binds with return type
  def !![A, B](f: A => B)(a: OpMonad[A]): OpMonad[B] = !!((a: A) => OpMonad(f(a)))(a)

  def !![A, B, C](f: (A, B) => C)(a: OpMonad[A], b: OpMonad[B]): OpMonad[C] = !!((a: A, b: B) => OpMonad(f(a, b)))(a, b)

  def !![A, B, C, D](f: (A, B, C) => D)(a: OpMonad[A], b: OpMonad[B], c: OpMonad[C]): OpMonad[D] =
    !!((a: A, b: B, c: C) => OpMonad(f(a, b, c)))(a, b, c)

  def !![A, B, C, D, E](f: (A, B, C, D) => E)(a: OpMonad[A], b: OpMonad[B], c: OpMonad[C], d: OpMonad[D]): OpMonad[E] =
    !!((a: A, b: B, c: C, d: D) => OpMonad(f(a, b, c, d)))(a, b, c, d)

  // Transform OpMonand[Operation] to Operation
  def opMonad2Operation[A](m: OpMonad[Pencil.Operation]) = new Pencil.BlockOperation(m.ops :+ m.value)

  def !!![A](f: A => Pencil.Operation): OpMonad[A] => Pencil.Operation = (a: OpMonad[A]) => opMonad2Operation(!!(f)(a))

  def !!![A, B](f: (A, B) => Pencil.Operation): (OpMonad[A], OpMonad[B]) => Pencil.Operation = (a, b) => opMonad2Operation(!!(f)(a, b))

  def !!![A, B, C](f: (A, B, C) => Pencil.Operation): (OpMonad[A], OpMonad[B], OpMonad[C]) => Pencil.Operation =
    (a, b, c) => opMonad2Operation(!!(f)(a, b, c))
}

case class Memoize[Key, Value](function: Key => Value) extends Function1[Key, Value] {
   private val map = new scala.collection.mutable.HashMap[Key, Value]
   def apply(key: Key): Value = synchronized { map.getOrElseUpdate(key, function(key)) }
}
