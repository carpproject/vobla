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
import com.arm.carp.pencil.Assertable
import com.arm.carp.pencil.Common
import com.arm.carp.pencil.CommonOps
import com.arm.carp.{vobla => Vobla}

// Define the base constant and types equivalent for VOBLA in PENCIL. Does not make any assumptions about ValueType
class Transformer extends Common with Assertable with CommonOps {
  protected val FloatType = Pencil.FloatType(32, false)
  protected val DoubleType = Pencil.FloatType(64, false)
  protected val IndexType = Pencil.IntegerType(true, 32, false)
  protected val IndexZero = Pencil.Constants.Integer32Constant0
  protected val IndexOne = Pencil.Constants.Integer32Constant1

  protected def isComplex(t: Pencil.Type) = t match {
    case t: Pencil.StructType => true
    case _ => false
  }

  protected def isComplex(t: Pencil.Type*): Boolean = t.exists(t => isComplex(t))
}
