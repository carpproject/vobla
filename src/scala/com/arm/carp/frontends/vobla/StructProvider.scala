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

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.HashMap
import com.arm.carp.{pencil => Pencil}
import com.arm.carp.{vobla => Vobla}
import scala.collection.mutable.ArrayBuffer

class StructProvider extends Transformer {
  private case class StorageVersion(s: Vobla.Storage, valueType: Vobla.ValueVersion.ValueVersion)
  private case class TypeVersion(t: Vobla.VoblaType, valueType: Vobla.ValueVersion.ValueVersion)

  private val ComplexFloat = Pencil.StructType(List(("Re", FloatType), ("Im", FloatType)), false, "ComplexFloat")
  private val ComplexDouble = Pencil.StructType(List(("Re", DoubleType), ("Im", DoubleType)), false, "ComplexDouble")

  def ComplexFloatType = use(ComplexFloat)
  def ComplexDoubleType = use(ComplexDouble)

  private val structFinder = HashMap[(List[(String, Pencil.Type)], Boolean) , Pencil.StructType]()
  private val pencilStructures = ArrayBuffer[Pencil.StructType]()
  private val storages = HashMap[StorageVersion, (Pencil.StructType, HashMap[Vobla.Variable, Int])]()
  private val types = HashMap[TypeVersion, (Pencil.StructType, HashMap[Vobla.Variable, Int])]()
  private val baseArrayView = HashMap[Int, Pencil.StructType]()
  private val arrayView = HashMap[(Int, Int), Pencil.StructType]()
  private var structId = 0;

  def use(s: Pencil.StructType): Pencil.StructType = structFinder.get((s.fields.toList, s.const)) match {
    case Some(s) => s
    case None =>
      // First, add the dependencies
      s.fields.foreach{
        case (_, x: Pencil.StructType) => use(x)
        case _ => ()
      }
      // then add the structure
      pencilStructures += s
      structFinder += (((s.fields.toList, s.const), s))
      s
  }

  def getStructures = pencilStructures.toList

  private[vobla] def getType(t: Vobla.VoblaType, valueType: Vobla.ValueVersion.ValueVersion, conjugated: Boolean,
      restrict: Boolean, specType: Vobla.ScalarType) = {
    val (typeStruct, typePos) = getTypeStructure(t, valueType)
    val (_, storagePos) = getStorageStructure(t.storage, valueType)
    val transformer = new StructTransformer(valueType, this, restrict)
    val members = t.storage.members.locals
    val (viewStruct, viewVars, viewVarTable) = transformer.transformStruct(typeStruct,
        storagePos.toMap, typePos.toMap, members, t.storage.sizes, specType.assignable)
    new ConcreteInterfaceView(t.dim, viewStruct, viewVars, viewVarTable, t, specType, conjugated, restrict)
  }

  private def getTypeStructure(t: Vobla.VoblaType, valueType: Vobla.ValueVersion.ValueVersion) = {
    val version = TypeVersion(t, valueType)
    types.get(version) match {
      case Some(t) => t
      case None =>
        val transformer = new StructTransformer(valueType, this)
        val (storageStructure, storagePos) = getStorageStructure(t.storage, valueType)
        val name = genStructName(t.name)
        val allMembers = t.members.toSet
        val typeMembers = allMembers -- storagePos.keySet
        val (struct, pos) = transformer.transformStructType(List(("storage", storageStructure)), typeMembers.toList, name, Map.empty)
        val realStruct = use(struct)
        types.put(version, (realStruct, pos))
        (realStruct, pos)
    }
  }

  private def getStorageStructure(s: Vobla.Storage, valueType: Vobla.ValueVersion.ValueVersion) = {
    val version = StorageVersion(s, valueType)
    storages.get(version) match {
      case Some(s) => s
      case None =>
        val transformer = new StructTransformer(valueType, this)
        val name = genStructName(s.name)
        val (struct, pos) = transformer.transformStructType(List(), s.members.locals, name, Map.empty)
        val realStruct = use(struct.updateConst(false))
        storages.put(version, (realStruct, pos))
        (realStruct, pos)
    }
  }

  private def genStructName(name: String) = {
    val newName = name + "_" + structId
    structId += 1
    newName
  }

  def getArrayViewStruct(baseDim: Int, viewDim: Int) = {
    arrayView.get((baseDim, viewDim)) match {
      case Some(s) => s
      case None =>
        val baseStruct = baseArrayView.get(baseDim) match {
          case Some(s) => s
          case None =>
            val members = List.tabulate(baseDim)(i => ("base_size" + i, IndexType))
            val s = Pencil.StructType(members , false, genStructName("ArrayBaseView"))
            baseArrayView += ((baseDim, s))
            s
        }
        val members = ("storage", baseStruct) :: (List.tabulate(viewDim)(i => ("view_size" + i, IndexType))
            ::: List.tabulate(baseDim)(i => ("offset" + i, IndexType)))
        val s = use(Pencil.StructType(members, false, genStructName("ArrayView")))
        arrayView += (((baseDim, viewDim), s))
        s
    }
  }

  def getArrayViewMembers(v: Pencil.ScalarVariable, baseDim: Int, dim: Int) = {
    val raw = Pencil.ScalarStructSubscription(v, 0)
    val baseSizes = List.tabulate(baseDim)(i => Pencil.ScalarStructSubscription(raw, i))
    val sizes = List.tabulate(dim)(i => Pencil.ScalarStructSubscription(v, i + 1))
    val offsets = List.tabulate(baseDim)(i => Pencil.ScalarStructSubscription(v, i + dim + 1))
    (baseSizes, sizes, offsets)
  }
}
