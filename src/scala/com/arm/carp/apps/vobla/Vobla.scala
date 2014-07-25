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

package com.arm.carp.apps.vobla

import com.arm.carp.apps.Version
import com.arm.carp.pencil.Printer
import java.io.PrintWriter
import java.io.File
import com.arm.carp.frontends.vobla.VoblaFrontEnd
import com.arm.carp.pencil.Checkable
import com.arm.carp.pencil.Lowering
import scala.collection.mutable.ListBuffer

/**
  * VOBLA-to-PENCIL compiler driver.
  *
  * Translates supplied VOBLA program into a PENCIL program.
  */
object Main {

  var inputFileName: Option[String] = None
  var outputFileName: Option[String] = None
  private var importPaths = new ListBuffer[String]

  /**
   * Parse command line arguments.
   * @param args - Command line arguments.
   * @return <code>true</code> if arguments are valid, <code>false</code> otherwise.
   */
  private def parseCommandLine(args: List[String]): Boolean = {
    args match {
      case Nil => inputFileName.isDefined
      case "-h" :: rest =>
        sayHelp(); false
      case "--version" :: rest =>
        sayVersion(); false
      case "-I" :: x :: rest =>
        importPaths += x; parseCommandLine(rest)
      case "-o" :: x :: rest =>
        outputFileName = Some(x); parseCommandLine(rest)
      case x :: rest => inputFileName = Some(x); parseCommandLine(rest)
    }
  }

  private def sayHelp() {
    System.err.println("Usage: input-files [-I import-path] [-o output-file]")
    System.exit(0)
  }

  private def sayVersion() {
    System.err.println("VOBLA Compiler")
    System.err.println(Version.getFullVersionInfo)
    System.exit(0)
  }

  private def complain(message: String) {
    System.err.println("Error:" + message)
    System.exit(-1)
  }

  /**
   * Add the current directory as the first path to be searched, add a
   * file/directory separator to the paths that lack it, and then return
   * a list of import paths.
   */
  private def getImportPaths(): List[String] = {
    "./" +=: importPaths
    importPaths.toList map(s =>
      if (s endsWith File.separator) s
      else s + File.separator
    )
  }

  def main(args: Array[String]) {
    if (parseCommandLine(args.toList) != true) {
      complain("Invalid command line arguments (use -h for help).")
    }

    val frontend = new VoblaFrontEnd(getImportPaths)
    val pencil = frontend.parse(inputFileName.get)

    if (pencil.isEmpty) {
      System.exit(-1)
    }

    /** PENCIL code, obtained from the front-end must be canonicalized. */
    val lowered = Lowering.walkProgram(pencil.get)
    Checkable.walkProgram(lowered)
    val writer = new Printer

    val code = writer.toPencil(pencil.get, true, true)

    outputFileName match {
      case Some(fname) =>
        val file = new PrintWriter(new File(fname))
        file.append(code)
        file.close
      case None =>
        println(code)
    }
  }
}
