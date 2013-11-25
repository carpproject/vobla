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
import com.arm.carp.vobla.ComparisonOp.ComparisonOp
import com.arm.carp.vobla.ValueVersion.ValueVersion
import com.arm.carp.frontends.vobla.Memoize
import scala.collection.mutable.ArrayBuffer

class Program(
    val functions: Map[String, VoblaFunction],
    val storages: Map[String, Storage],
    val interfaces: Map[(String, Int), Interface],
    val types: Map[String, MethodImplementerVersion],
    val views: Map[String, VoblaView],
    exportsMaker: => List[(String, FunctionVersion)]) {
  def interfaceBodies = interfaces.values.map(i => (i.subInterfaces, i.properMethods))
  def typeDecls = {
    types.values.map{
      case TypeVersion(t) => (t.storage, t.members, t.subInterfaces)
      case _ =>
    }
    views.values.map(v => (v.interface, v.subInterfaces))
  }
  def functionDecls = functions.values.map(f => (f.decl.args, f.decl.returnType))
  def methods = types.values.map(_.base.methods); views.values.map(v => (v.base, v.methods))
  def functionBodies = functions.values.map(f => f.body)
  def exports = exportsMaker
}

class FunctionDecl(val name: String, rType: => Type, argsMaker: => List[Type]) {
  lazy val returnType = rType
  lazy val args = argsMaker

  override def equals(x: Any) = x match {
    case decl: FunctionDecl => decl.name == name
    case _ => false
  }

  override def hashCode() = name.hashCode

  def checkCall(callArgs: List[Expression], spec: ScalarType) =
    args.size == callArgs.size && (args, callArgs).zipped.forall((t, e) => e.expType > t.specialize(spec))

  def getSpecializedType(callArgs: List[Expression]) =
    (args, callArgs.map(_.expType)).zipped.map{
      case (InterfaceType(aInter, aBase), InterfaceType(inter, base)) => (aBase, inter.subInterfaces.get(aInter) match {
        case Some(ib) => ib.specialize(base)
        case None => base
      })
      case (a, t) => (a.base, t.base)
    }.map{
      case (_: ValueType, _: ComplexType) => ComplexType(true)
      case (_: ValueType, _: ValueType) => ValueType(true)
      case _ => RealType(true)
    }.foldLeft[ScalarType](RealType(true)){
      case (_: ComplexType, _) | (_, _: ComplexType) => ComplexType(true)
      case (_: ValueType, _) | (_, _: ValueType) => ValueType(true)
      case (_: RealType, _: RealType) | (_: IndexType, _) | (_, _: IndexType) => RealType(true)
    }

  val hasSideEffect = args.exists{
    case _: ScalarType => false
    case t => t.assignable
  }

  override def toString() = name + "(" + args.map(_.toString).reduceOption(_ + ", " + _).getOrElse("") + "): " + returnType
}

object BuiltInInterfaces {
  case class GetRange(dim: Int) extends FunctionDecl("getRange", RangeType(List.fill(dim)(IndexType())), List())
  case class Iterate(dim: Int) extends FunctionDecl("iterate", RangeType(List.fill(dim)(IndexType()) :+ ValueType(true)), List())
  case class SparseIterate(dim: Int) extends FunctionDecl("sparse", RangeType(List.fill(dim)(IndexType()) :+ ValueType(true)), List())
  case class Access(dim: Int) extends FunctionDecl("access", ValueType(true), List.fill(dim)(IndexType()))
  case class GetLen(dim: Int) extends FunctionDecl("getLen" + dim, IndexType(), List()) {
    assert(dim > 0)
  }
  case class Re(dim: Int) extends FunctionDecl("re", InterfaceType(array(dim), RealType(true)), List())
  case class Im(dim: Int) extends FunctionDecl("im", InterfaceType(array(dim), RealType(false)), List())

  // interfaces
  val sparseIterable = (dim: Int) => new Interface("SparseIterable", Map(measurable(dim) -> ValueType(true)), Set(SparseIterate(dim)), dim)
  val iterable = (dim: Int) => new Interface("Iterable", Map(measurable(dim) -> ValueType(true), sparseIterable(dim) -> ValueType(true)),
      Set(Iterate(dim)), dim)
  val sparseArray = (dim: Int) =>
    new Interface("SparseArray", Map(measurable(dim) -> ValueType(true), iterable(dim) -> ValueType(false),
        sparseIterable(dim) -> ValueType(true)), Set(), dim)
  val rangeable = (dim: Int) => new Interface("Rangeable", Map(measurable(dim) -> ValueType(true)), Set(GetRange(dim)), dim)
  val measurable = (dim : Int) => new Interface("Measurable", Map(), List.tabulate(dim)(i => GetLen(i+1)).toSet, dim)
  val randomAccessible = (dim: Int) =>
    new Interface("RandomAccessible", Map(measurable(dim) -> ValueType(true)), Set(Access(dim)), dim)
  val accessible = (dim: Int) => new Interface("Accessible", Map(randomAccessible(dim) -> ValueType(true), iterable(dim) -> ValueType(true)), Set(), dim)
  val complexObject = Memoize[Int, Interface](dim => new Interface("ComplexObject", Map(measurable(dim) -> ValueType(true)), Set(Re(dim), Im(dim)), dim))
  val array = Memoize[Int, Interface](dim => new Interface("Array", arrayImplements.map(i => i(dim) -> ValueType(true)).toMap, Set(), dim))

  private def arrayImplements = List(iterable, sparseArray, rangeable, measurable, randomAccessible, complexObject, sparseIterable, accessible)
  val interfaces = (array::arrayImplements).map(f => f(0).name -> f).toMap
}

// Methods and functions
case class VoblaFunction(decl: FunctionDecl, vars: VarTable, bodyMaker: () => List[Operation]) {
  lazy val body = bodyMaker()
}

trait Method { val decl: FunctionDecl; val args: List[Variable]; val locals: List[Variable] }
case class MethodImpl(decl: FunctionDecl, args: List[Variable], locals: List[Variable], body: List[Operation],
    returnOp: ReturnOperation) extends Method
case class RangeImpl(decl: FunctionDecl, args: List[Variable], locals: List[Variable], body: List[Operation]) extends Method

case class Storage(name: String, members: VarTable, sizes: Map[Variable, List[Expression]])

class Interface(val name: String, subInterfacesMaker: => Map[Interface, ScalarType], properMethodsMaker: => Set[FunctionDecl], d: => Int) {
  lazy val subInterfaces = subInterfacesMaker
  lazy val methodDecls = properMethods.map(_ -> ValueType(true)) ++: subInterfaces.flatMap{ case (i, base) => i.properMethods.map(_ -> base) }
  lazy val methodsDeclByName = methodDecls.map(m => (m._1.name, m)).toMap
  lazy val properMethods = properMethodsMaker
  lazy val dim = d
  lazy val allInterfaces = subInterfaces + (this -> ValueType(true))

  override def equals(x: Any) = x match {
    case x: Interface => x.name == name && x.dim == dim
    case _ => false
  }

  override def hashCode() = name.hashCode + dim.hashCode

  override def toString = name + List.fill(dim)("[]").reduceOption(_+_).getOrElse("")
}

class MethodImplementer(name: String, subInterfacesMaker: => Map[Interface, ScalarType], methodsMaker: MethodImplementer => Map[FunctionDecl, Method])
  extends Interface(name, subInterfacesMaker, Set(), subInterfacesMaker.keySet.toList.map(_.dim).reduceOption(math.max).getOrElse(0)) {
  lazy val methods = methodsMaker(this)
  lazy val thisVar = Variable("this", InterfaceType(this, ValueType(true)))
}

class VoblaType(name: String, storageMaker: => Storage, membersMaker: => List[Variable], subInterfacesMaker: => Map[Interface, ScalarType],
    methodsMaker: MethodImplementer => Map[FunctionDecl, Method]) extends MethodImplementer(name, subInterfacesMaker, methodsMaker) {
  lazy val storage = storageMaker
  lazy val members = membersMaker
}

class VoblaView(name: String, interfaceMaker: => Interface, baseMaker: => Variable, subInterfacesMaker: => Map[Interface, ScalarType],
    methodsMaker: MethodImplementer => Map[FunctionDecl, Method]) extends MethodImplementer(name, subInterfacesMaker, methodsMaker) {
  lazy val interface = interfaceMaker
  lazy val base = baseMaker
}

// Operations
trait Operation

trait MovOp extends Operation {
  val lValue: Expression;
  val rValue: Expression;
  val leftIterate: Boolean;
  val sparseIterate: Boolean;
}

case class ReturnOperation(expression: Option[Expression], expType: Type) extends Operation
case class YieldOperation(expressions: List[Expression]) extends Operation
case class IfOperation(condition: Condition, ifBody: List[Operation], elseBody: List[Operation]) extends Operation
case class WhileOperation(condition: Condition, body: List[Operation]) extends Operation
case class ForOperation(iterations: List[Iteration], body: List[Operation], parallel: Boolean) extends Operation
case class CallOperation(expression: CallExpression) extends Operation
case class MovOperation(lValue: Expression, rValue: Expression, isInit: Boolean, leftIterate: Boolean, sparseIterate: Boolean) extends MovOp
case class MovPlusOperation(lValue: Expression, rValue: Expression, leftIterate: Boolean, sparseIterate: Boolean) extends MovOp
case class MovMinusOperation(lValue: Expression, rValue: Expression, leftIterate: Boolean, sparseIterate: Boolean) extends MovOp
case class MovMulOperation(lValue: Expression, rValue: Expression, leftIterate: Boolean, sparseIterate: Boolean) extends MovOp
case class MovDivOperation(lValue: Expression, rValue: Expression, leftIterate: Boolean, sparseIterate: Boolean) extends MovOp
case class AssumeOperation(cond: Condition) extends Operation
trait ArrayDeclOperation extends Operation {val variable: Variable}
case class TypeofArrayDecl(variable: Variable, exp: Expression, assign: Boolean = false) extends ArrayDeclOperation
case class ArrayDecl(variable: Variable, baseType: ScalarType, sizes: List[Expression]) extends ArrayDeclOperation

// Boolean expressions
trait Condition
case class LogicalOr(exp0: Condition, exp1: Condition) extends Condition
case class LogicalAnd(exp0: Condition, exp1: Condition) extends Condition
case class LogicalNot(exp: Condition) extends Condition
case class Comparison(op: ComparisonOp, exp0: Expression, exp1: Expression) extends Condition

object ComparisonOp extends Enumeration {
  type ComparisonOp = Value
  val EQ, NEQ, MORE, LESS, MOREEQ, LESSEQ = Value
}

// Offsets
trait Offset
case class FullSlice() extends Offset
case class Slice(low: Expression, up: Expression) extends Offset
case class Idx(idx: Expression) extends Offset

// Expressions
trait Expression { val expType: Type }
trait OneOpExpression extends Expression { val base: Expression }
trait TowOpExpression extends Expression { val exp0: Expression; val exp1: Expression }
trait TwoOpVecOp extends TowOpExpression { val left: Boolean }
trait Constant extends Expression

case class IndexConstant(i: Int) extends Constant { override val expType = IndexType() }
case class RealConstant(r: Double) extends Constant { override val expType = RealType() }
case class ComplexConstant(re: Double, im: Double) extends Constant { override val expType = ComplexType() }
case class ArrayConstant(values: List[Constant], override val expType: ArrayType) extends Constant

case class ConstExpression(base: Expression) extends OneOpExpression { override val expType = base.expType.setAssignable(false) }
case class VecMultExpression(exp0: Expression, exp1: Expression, override val expType: Type) extends TowOpExpression
case class PlusExpression(exp0: Expression, exp1: Expression, override val expType: Type, left: Boolean) extends TwoOpVecOp
case class MinusExpression(exp0: Expression, exp1: Expression, override val expType: Type, left: Boolean) extends TwoOpVecOp
case class UMinusExpression(base: Expression, expType: Type) extends OneOpExpression
case class MultExpression(exp0: Expression, exp1: Expression, override val expType: Type) extends TowOpExpression
case class DivExpression(exp0: Expression, exp1: Expression, override val expType: Type) extends TowOpExpression

case class ApplyConjugatedExpression(base: Expression ) extends OneOpExpression { override val expType = base.expType }
case class ReExpression(base: Expression) extends OneOpExpression { override val expType = RealType(base.expType.assignable)}
case class ImExpression(base: Expression) extends OneOpExpression { override val expType = RealType(base.expType.assignable)}
case class ReversedExpression(base: Expression) extends OneOpExpression { override val expType = base.expType}
case class ConjExpression(base: Expression) extends OneOpExpression { override val expType = base.expType}
case class DiagExpression(base: Expression, override val expType: Type) extends OneOpExpression
case class AntiDiagExpression(base: Expression, override val expType: Type) extends OneOpExpression
case class TransposedExpression(base: Expression) extends OneOpExpression { override val expType = base.expType }
case class OffsetExpression(base: Expression, offsets: List[Offset], override val expType: Type) extends OneOpExpression

case class ConstructorExpression(t: VoblaType, args: List[Expression]) extends Expression {
  override val expType = InterfaceType(t, ValueType(true))
}

case class ViewConstructorExpression(view: VoblaView, base: Expression, expType: InterfaceType) extends OneOpExpression

case class SliceRange(low: Expression, up: Expression, step: Int) extends Expression {
  override val expType = RangeType(List(IndexType()))
}

case class ObjScalarMultExpression(vector: Expression, scalar: Expression, expType: Type) extends Expression

case class CallExpression(function: VoblaFunction, args: List[Expression], specType: ScalarType) extends Expression {
  override val expType = function.decl.returnType.specialize(specType)
}

case class MethodCallExpression(base: Expression, m: FunctionDecl, a: List[Expression], t: ScalarType)
  extends BaseMethodCallExpression(m, a, t) with OneOpExpression

class BaseMethodCallExpression(val method: FunctionDecl, val args: List[Expression], val specType: ScalarType) extends Expression {
  override val expType = method.returnType.specialize(specType)
}

case class IntrinsicCallExpression(name: String, args: List[Expression]) extends Expression {
  override val expType = args.head.expType.setAssignable(false)
}

case class SumExpression(iteration: Iteration, base: Expression) extends OneOpExpression {
  override val expType = base.expType.asInstanceOf[ScalarType].setAssignable(false)
}

// Objects
case class Iteration(variables: List[Option[Variable]], range: Expression)

object ArrayPart extends Enumeration {
  type ArrayPart = Value
  val Full, Re, Im, Zero = Value
  def combine(op0: ArrayPart, op1: ArrayPart) = (op0, op1) match {
    case (Full, op) => op
    case (op, Full) => op
    case (Re, Re) => Re
    case (Im, Im) => Im
    case _ => Zero
  }
}

trait ArgVersion
case class FunctionVersion(valueVersion: ValueVersion.ValueVersion, function: VoblaFunction, args: List[ArgVersion])
case class InterfaceVersion(t: MethodImplementerVersion, conjugated: Boolean, restrict: Boolean) extends ArgVersion
case class ScalarVersion(conjugated: Boolean) extends ArgVersion
case class ArrayVersion(conjugated: Boolean, restrict: Boolean, arrayPart: ArrayPart.ArrayPart,
     factors: List[List[Int]], reverseSizes: Boolean = false) extends ArgVersion with ViewArgVersion

trait ViewArgVersion
trait MethodImplementerVersion extends ViewArgVersion { val base: MethodImplementer }
case class TypeVersion(base: VoblaType) extends MethodImplementerVersion
case class ViewVersion(base: VoblaView, subType: ViewArgVersion) extends MethodImplementerVersion

object ValueVersion extends Enumeration {
  type ValueVersion = Value
  val FLOAT, DOUBLE, COMPLEX_DOUBLE, COMPLEX_FLOAT = Value
}

// Variables
object Variable {
  private var nextId = -1
  def apply(name: String, t: Type) = {
    nextId += 1
    new Variable(name, t, nextId)
  }
}

case class Variable(val name: String, override val expType: Type, val id: Int) extends Expression
