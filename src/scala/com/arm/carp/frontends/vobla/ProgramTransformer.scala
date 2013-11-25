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

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

import com.arm.carp.{ pencil => Pencil }
import com.arm.carp.pencil.Checkable
import com.arm.carp.{ vobla => Vobla}

/**
 * Translates a VOBLA program to a PENCIL program.
 */
class ProgramTransformer extends Transformer {

  private val structures = new StructProvider()
  private val functionVersions = new HashMap[Vobla.FunctionVersion, Set[Pencil.Function]] with MultiMap[Vobla.FunctionVersion, Pencil.Function]

  private val functionsToImplement = Queue[(Vobla.FunctionVersion, Pencil.Function, FunctionTransformer)]()

  def transformProgram(versions: List[(String, Vobla.FunctionVersion)]): Pencil.Program = {
    versions.foreach{ case (name, version) => registerExportedFunction(version.function, name, version) }
    implementFunctions
    new Pencil.Program(functionVersions.values.flatten, structures.getStructures, List())
  }

  private def registerExportedFunction(f: Vobla.VoblaFunction, name: String, version: Vobla.FunctionVersion) = {
    val transformer = new FunctionTransformer(version.valueVersion, structures, functionVersions, functionsToImplement)
    val function = transformer.transformFunctionDeclaration(name, version, false)
    functionVersions.addBinding(version, function)
    functionsToImplement.enqueue((version, function, transformer))
  }

  private def implementFunctions = {
    while(functionsToImplement.size > 0) {
     val (version, function, transformer) = functionsToImplement.dequeue
     function.ops = Some(transformer.transformBody(version.function.body))
    }
  }
}
