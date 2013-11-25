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

import com.arm.carp.pencil.Program
import com.arm.carp.{ vobla => Vobla }
import org.antlr.runtime.ANTLRFileStream
import org.antlr.runtime.CommonTokenStream
import com.arm.carp.vobla.parser.voblaLexer
import com.arm.carp.vobla.parser.voblaParser
import scala.collection.JavaConversions._
import java.io.File
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class UserError(val message: String) extends Throwable
case class HandledError() extends Throwable

class VoblaFrontEnd {

  private val files = HashMap[String, Vobla.Program]()

  def parse(file: String): Option[Program] = {
    try {
      val baseProgram = parseFile(Set())(file)
      files.values.foreach(p => p.interfaceBodies)
      files.values.foreach(p => p.typeDecls)
      files.values.foreach(p => p.functionDecls)
      files.values.foreach(p => p.methods)
      files.values.foreach(p => p.functionBodies)
      files.values.foreach(p => p.exports)
      val transformer = new ProgramTransformer
      Some(transformer.transformProgram(baseProgram.exports))
    } catch {
      case HandledError() => None
      case e: UserError => System.err.println(e.message)
        None
        //throw e
      case e: Throwable => throw e
    }
  }

  private def parseFile(branch: Set[String])(file: String): Vobla.Program = {
    val f = new File(file)
    if (!f.exists())
      throw new UserError("File " + file + " not found")
    val path = f.getCanonicalPath()
    if(branch.contains(path))
      throw new UserError("File " + file + " depends on itself for import")
    files.get(path) match {
      case Some(f) => f
      case None =>
        val input = new ANTLRFileStream(file)
        val lexer = new voblaLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new voblaParser(tokens)
        val tree = parser.program
        if(!parser.isCorrect) throw HandledError()
        val ast = Ast(tree)
        val imps = ast.imports.map(makeFileName).map(parseFile(branch + path))
        val (funs, names0) = imps.map(i => i.functions).foldLeft((HashMap[String, Vobla.VoblaFunction](), List[String]())){
          case ((m1, names), m2) => (m1 ++ m2, names ++ m2.keys)
        }
        val (storages, names1) = imps.map(i => i.storages).foldLeft((HashMap[String, Vobla.Storage](),names0)){
          case ((m1, names), m2) => (m1 ++ m2, names ++ m2.keys)
        }
        val (types, names2) = imps.map(i => i.types).foldLeft((HashMap[String, Vobla.MethodImplementerVersion](), names1)){
          case ((m1, names), m2) => (m1 ++ m2, names ++ m2.keys)
        }
        val (views, names3) = imps.map(i => i.views).foldLeft((HashMap[String, Vobla.VoblaView](), names2)){
          case ((m1, names), m2) => (m1 ++ m2, names ++ m2.keys)
        }
        val (interfaces, interIds) = imps.map(i => i.interfaces).foldLeft((HashMap[(String, Int), Vobla.Interface](), List[(String, Int)]())){
          case ((m1, names), m2) => (m1 ++ m2, names ++ m2.keys)
        }
        val duplicateInter = interIds.diff(interIds.distinct)
        val names = interIds.distinct.map(_._1) ++ names3
        if(!duplicateInter.isEmpty) {
          val (name, dim) = duplicateInter.head
          throw new UserError("In file " + path + ", interface " + name +
              List.fill(dim)("[]").reduceLeft((s1, s2) => s1 + s2) + " is imported twice")
        }
        val duplicateNames = names.diff(names.distinct)
        if(!duplicateNames.isEmpty)
          throw new UserError("In file " + path + ", " + duplicateNames.head + " is imported twice")
        val typer = new Typer(file, funs, storages, interfaces, types, views)
        val p = typer.typeVoblaProgram(ast)
        files += ((path, p))
        p
    }
  }

  private def makeFileName(imp: Ast.Import) = imp.file.map(x => x.text).reduceLeft((x, y) => x + "/" + y) + ".vobla"
}
