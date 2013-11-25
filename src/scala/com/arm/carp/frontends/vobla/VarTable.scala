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

package com.arm.carp.vobla

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class VarTable(val parent: Option[VarTable] = None) {
  parent.map(p => p.childs += this)

  private val childs = ArrayBuffer[VarTable]()
  private val variables = HashMap[String, Expression]()
  private val localVars = ArrayBuffer[Variable]()

  def locals = localVars.toList
  def subVariables: List[Variable] = locals ++ childs.flatMap(c => c.subVariables)
  def allVariables: List[Variable] = locals ++ parent.map(_.allVariables).getOrElse(Nil)
  def strictSubVariables = childs.flatMap(c => c.subVariables)
  def branch = new VarTable(Some(this))

  def get(name: String): Option[Expression] = variables.get(name) match {
    case None => parent.flatMap(p => p.get(name))
    case e => e
  }

  def contains(name: String): Boolean = variables.contains(name) || parent.exists(t => t.contains(name))

  def addExpr(name: String, e: Expression) = {
    if(contains(name)) false
    else {
      variables.put(name, e)
      true
    }
  }

  def add(v: Variable) = {
    if(contains(v.name)) None
    else {
      variables.put(v.name, v)
      localVars += v
      Some(v)
    }
  }
}
