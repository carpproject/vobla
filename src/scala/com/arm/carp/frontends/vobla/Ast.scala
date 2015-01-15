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

import com.arm.carp.{ vobla => Vobla }
import com.arm.carp.vobla.parser.voblaParser._
import com.arm.carp.pencil.Assertable
import com.arm.carp.pencil.Common
import scala.collection.mutable.ArrayBuffer
import org.antlr.runtime.tree.Tree
import scala.reflect.ClassTag

private object Ast extends Common with Assertable {
  trait Ast { val tree: Tree }
  trait Op2ExpressionAst extends Ast { val exp0: Ast; val exp1: Ast }
  trait MovOp extends Ast { val lValue: Ast; val rValue: Ast}

  case class GenericAst[+T](value: T, tree: Tree) extends Ast
  case class ParserList(list: List[Ast], tree: Tree) extends Ast // TODO: Forbid this in the final Ast
  case class Suffix(parallel: Boolean, iters: List[Iteration], tree: Tree) extends Ast

  case class Import(file: List[Name], tree: Tree) extends Ast
  case class Program(imports: List[Import], delarations: List[Ast], tree: Tree) extends Ast
  case class Function(name: Name, params: List[Ast], returnType: Ast, body: List[Ast], tree: Tree) extends Ast
  case class Export(funName: Name, valueType: Vobla.ValueVersion.ValueVersion, params: List[ArgumentType], name: Name, tree: Tree) extends Ast
  case class Storage(name: Name, members: List[Member], tree: Tree) extends Ast
  case class InterfaceId(name: Name, dim: Int, tree: Tree) extends Ast
  case class InterfaceType(interface: InterfaceId, base: Vobla.ScalarType, tree: Tree) extends Ast
  case class Interface(name: Name, implements: List[InterfaceType], methods: List[MethodHdr], dim: Int, tree: Tree) extends Ast
  case class Type(name: Name, storage: Name, implements: List[InterfaceType], members: List[Member], methods: List[Method],
      tree: Tree) extends Ast
  case class TypeDef(name: Name, version: TypeVersion, tree: Tree) extends Ast
  case class View(name: Name, interface: InterfaceId, implements: List[InterfaceType], methods: List[Method], tree: Tree) extends Ast
  case class RangeType(subTypes: List[Ast], tree: Tree) extends Ast
  case class ArgumentType(name: Name, qualifiers: List[Ast], tree: Tree) extends Ast
  case class Void(tree: Tree) extends Ast
  case class Name(text: String, tree: Tree) extends Ast
  case class TypeVersion(name: Name, subVersion: Option[TypeVersion], tree: Tree) extends Ast
  case class IndexConstant(i: Int, tree: Tree) extends Ast
  case class RealConstant(r: Double, tree: Tree) extends Ast
  case class PlusExpression(exp0: Ast, exp1: Ast, tree: Tree) extends Op2ExpressionAst
  case class UMinusExpression(base: Ast, tree: Tree) extends Ast
  case class MinusExpression(exp0: Ast, exp1: Ast, tree: Tree) extends Op2ExpressionAst
  case class MultExpression(exp0: Ast, exp1: Ast, tree: Tree) extends Ast
  case class DivExpression(exp0: Ast, exp1: Ast, tree: Tree) extends Op2ExpressionAst
  case class SliceRange(low: Ast, up: Ast, step: Int, tree: Tree) extends Ast
  case class SumExpression(iterations: List[Iteration], base: Ast, tree: Tree) extends Ast
  case class ArrayConstant(values: List[Ast], tree: Tree) extends Ast
  case class MethodCallExpression(base: Ast, method: Name, args: List[Ast], tree: Tree) extends Ast
  case class OffsetExpression(base: Ast, offsets: List[Ast], tree: Tree) extends Ast
  case class Member(name: Name, mType: Vobla.ScalarType, dims: List[Ast], tree: Tree) extends Ast
  case class Method(decl: MethodHdr, body: List[Ast], tree: Tree) extends Ast
  case class ArrayParam(name: Name, assignable: Boolean, baseType: Vobla.ScalarType, dims: List[Ast], tree: Tree) extends Ast
  case class ScalarParam(name: Name, t: Vobla.ScalarType, tree: Tree) extends Ast
  case class InterfaceParam(name: Name, assignable: Boolean, iName: Name, sType: Vobla.ScalarType, dims: List[Ast], tree: Tree) extends Ast
  case class MethodHdr(name: Name, arguments: List[ScalarParam], returnType: Ast, tree: Tree) extends Ast
  case class Yield(exps: List[Ast], tree: Tree, invConst: Boolean = true) extends Ast
  case class For(iters: List[Iteration], body: List[Ast], parallel: Boolean, tree: Tree) extends Ast
  case class If(cond: Ast, trueBody: List[Ast], falseBody: List[Ast], tree: Tree) extends Ast
  case class Return(value: Option[Ast], tree: Tree) extends Ast
  case class While(cond: Ast, body: List[Ast], tree: Tree) extends Ast
  case class Iteration(vars: List[Option[Name]], exp: Ast, tree: Tree) extends Ast
  case class Slice(low: Ast, up: Ast, tree: Tree) extends Ast
  case class Comparison(op: Vobla.ComparisonOp.ComparisonOp, exp0: Ast, exp1: Ast, tree: Tree) extends Ast
  case class And(exp0: Ast, exp1: Ast, tree: Tree) extends Ast
  case class Or(exp0: Ast, exp1: Ast, tree: Tree) extends Ast
  case class Not(exp: Ast, tree: Tree) extends Ast
  case class Mov(lValue: Ast, rValue: Ast, tree: Tree) extends MovOp
  case class MovPlus(lValue: Ast, rValue: Ast, tree: Tree) extends MovOp
  case class MovMinus(lValue: Ast, rValue: Ast, tree: Tree) extends MovOp
  case class MovMul(lValue: Ast, rValue: Ast, tree: Tree) extends MovOp
  case class MovDiv(lValue: Ast, rValue: Ast, tree: Tree) extends MovOp
  case class Len(obj: Ast, i: Option[IndexConstant], tree: Tree) extends Ast
  case class Call(name: Name, args: List[Ast], tree: Tree) extends Ast
  case class BuiltinCall(name: BuiltinMethod.BuiltinMethod, arg: Ast, tree: Tree) extends Ast
  case class VarDecl(name: Name, baseType: Vobla.ScalarType, dims: List[Ast], tree: Tree) extends Ast
  case class TypeOfDecl(name: Name, init: Ast, assign: Boolean, tree: Tree) extends Ast
  case class ArrayType(base: Vobla.ScalarType, dims: List[Ast], tree: Tree) extends Ast

  object BuiltinMethod extends Enumeration {
    type BuiltinMethod = Value
    val Conjugated, Transposed, Reversed, Diagonal, AntiDiagonal, Column, Row, Range, Re, Im, ApplyConjugated = Value
  }

  private def checkList[T: ClassTag](l: List[_]) = l.forall{ case _: T => true; case _ => false}
  private def castList[T](l: List[_]) = l.asInstanceOf[List[T]]

  private val getIterVar = (_: Ast) match {
    case GenericAst(None, _) => None
    case name: Name => Some(name)
  }

  private def makeAst(nodeData: Int, childs: List[Ast], t: Tree): Ast = (nodeData, childs) match {
    case (LIST, childs) => ParserList(childs, t)
    case (IMPORT, names) if checkList[Name](names) => Import(castList[Name](names), t)
    case (FUNCTION, List(name: Name, params: ParserList, returnType, ParserList(body, _))) => Function(name, params.list, returnType, body, t)
    case (EXPORT, List(funName: Name, GenericAst(valueType: Vobla.ValueVersion.ValueVersion, _), ParserList(params, _), name: Name))
      if checkList[ArgumentType](params) => Export(funName, valueType, castList[ArgumentType](params), name, t)
    case (STORAGE, (name: Name)::members) if checkList[Member](members) => Storage(name, castList[Member](members), t)
    case (INTERFACE, InterfaceId(name, dim, _)::ParserList(implements, _)::methods)
      if checkList[InterfaceType](implements) && checkList[MethodHdr](methods) =>
      Interface(name, castList[InterfaceType](implements), castList[MethodHdr](methods), dim, t)
    case (INTERFACE_NAME, GenericAst(false, _)::(name: Name)::GenericAst(base: Vobla.ScalarType, _)::dims) =>
      InterfaceType(InterfaceId(name, dims.size, name.tree), base.setAssignable(false), t)
    case (INTERFACE_NAME, (name: Name)::GenericAst(base: Vobla.ScalarType, _)::dims) =>
      InterfaceType(InterfaceId(name, dims.size, name.tree), base.setAssignable(true), t)
    case (INTERFACE_NAME, (name: Name)::dims) => InterfaceId(name, dims.size, name.tree)
    case (ARRAY_LEFT, List()) => GenericAst(None, t)
    case (TYPE,  List(name: Name, storage: Name, ParserList(implements, _), ParserList(members, _), ParserList(methods, _)))
        if checkList[InterfaceType](implements) && checkList[Member](members) && checkList[Method](methods) =>
      Type(name, storage, castList[InterfaceType](implements), castList[Member](members), castList[Method](methods), t)
    case (VIEW, (name: Name)::(interface: InterfaceId)::(ParserList(implements, _))::methods)
        if checkList[InterfaceType](implements) && checkList[Method](methods) =>
      View(name, interface, castList[InterfaceType](implements), castList[Method](methods), t)
    case (NAME, List()) => Name(t.getText, t)
    case (TYPEDEF, List(name: Name, version: TypeVersion)) => TypeDef(name, version, t)
    case (TYPE_VERSION, List(name: Name)) => TypeVersion(name, None, t)
    case (TYPE_VERSION, List(name: Name, subVersion: TypeVersion)) => TypeVersion(name, Some(subVersion), t)
    case (INT_NUMBER, List()) => IndexConstant(t.getText.toInt, t)
    case (DOUBLE_NUMBER, List()) => RealConstant(t.getText.toDouble, t)
    case (PLUS, List(e0, e1)) => PlusExpression(e0, e1, t)
    case (MINUS, List(e0, e1)) => MinusExpression(e0, e1, t)
    case (MINUS, List(e)) => UMinusExpression(e, t)
    case (MULT, List(e0, e1)) => MultExpression(e0, e1, t)
    case (DIV, List(e0, e1)) => DivExpression(e0, e1, t)
    case (SLICE, List(lower, upper)) => SliceRange(lower, upper, 1, t)
    case (SLICE, List(lower, upper, IndexConstant(by, _))) => SliceRange(lower, upper, by, t)
    case (SLICE, List(lower, upper, UMinusExpression(IndexConstant(by, _), _))) => SliceRange(lower, upper, -by, t)
    case (SUM, List(e, ParserList(iters, _))) if checkList[Iteration](iters) => SumExpression(castList[Iteration](iters), e, t)
    case (ARRAY_CST, values) => ArrayConstant(values, t)
    case (DOT, List(base, name: Name, arguments: ParserList)) => MethodCallExpression(base, name, arguments.list, t)
    case (DOT, List(base, name: Name)) => MethodCallExpression(base, name, Nil, t)
    case (OFFSET, e::offsets) => OffsetExpression(e, offsets, t)
    case (NONE, List()) => Void(t)
    case (IS, (name: Name) :: qualifiers) =>  ArgumentType(name, qualifiers, t)
    case (MEMBER, (name: Name)::GenericAst(mType: Vobla.ScalarType, _)::dims) => Member(name, mType, dims, t)
    case (METHOD, List(decl: MethodHdr, ParserList(body, _))) => Method(decl, body, t)
    case (ARRAY_PARAM, GenericAst(assignable: Boolean, _)::(name: Name)::GenericAst(baseType: Vobla.ScalarType, _)::dims) =>
      ArrayParam(name, assignable, baseType, dims, t)
    case (IN, List()) => GenericAst(false, t)
    case (OUT, List()) => GenericAst(true, t)
    case (SCALAR_PARAM, List(name: Name, GenericAst(sType: Vobla.ScalarType, _))) => ScalarParam(name, sType, t)
    case (INTERFACE_PARAM, GenericAst(assignable: Boolean, _)::(name: Name)::(iName: Name)::GenericAst(sType: Vobla.ScalarType, _)::dims) =>
      InterfaceParam(name, assignable, iName, sType, dims, t)
    case (METHOD_HDR, List(name: Name, ParserList(args, _), returnType)) if checkList[ScalarParam](args) =>
      MethodHdr(name, castList[ScalarParam](args), returnType, t)
    case (YIELD, List(ParserList(exps, _))) => Yield(exps, t)
    case (YIELD, List(ParserList(exps, _), Suffix(par, iters, _))) => For(iters, List(Yield(exps, t, false)), par, t)
    case (FOR, List(ParserList(iters, _), ParserList(body, _))) if checkList[Iteration](iters) => For(castList[Iteration](iters), body, false, t)
    case (FORALL, List(ParserList(iters, _), ParserList(body, _)))  if checkList[Iteration](iters) => For(castList[Iteration](iters), body, true, t)
    case (FOR, List(ParserList(iters, _))) if checkList[Iteration](iters) => Suffix(false, castList[Iteration](iters), t)
    case (FORALL, List(ParserList(iters, _))) if checkList[Iteration](iters) => Suffix(true, castList[Iteration](iters), t)
    case (IF, List(cond, ParserList(ops0, _), ParserList(ops1, _))) => If(cond, ops0, ops1, t)
    case (IF, List(cond, ParserList(ops0, _))) => If(cond, ops0, List(), t)
    case (RETURN, List(e)) => Return(Some(e), t)
    case (RETURN, List()) => Return(None, t)
    case (WHILE, List(cond, ParserList(body, _))) => While(cond, body, t)
    case (MOV, List(lValue, rValue)) => Mov(lValue, rValue, t)
    case (PLUS_MOV, List(lValue, rValue)) => MovPlus(lValue, rValue, t)
    case (MINUS_MOV, List(lValue, rValue)) => MovMinus(lValue, rValue, t)
    case (DIV_MOV, List(lValue, rValue)) => MovDiv(lValue, rValue, t)
    case (MULT_MOV, List(lValue, rValue)) => MovMul(lValue, rValue, t)
    case (MOV, List(lValue, rValue, Suffix(par, iters, _))) => For(iters, List(Mov(lValue, rValue, t)), par, t)
    case (PLUS_MOV, List(lValue, rValue, Suffix(par, iters, _))) => For(iters, List(MovPlus(lValue, rValue, t)), par, t)
    case (MULT_MOV, List(lValue, rValue, Suffix(par, iters, _))) => For(iters, List(MovMul(lValue, rValue, t)), par, t)
    case (DIV_MOV, List(lValue, rValue, Suffix(par, iters, _))) => For(iters, List(MovDiv(lValue, rValue, t)), par, t)
    case (MINUS_MOV, List(lValue, rValue, Suffix(par, iters, _))) => For(iters, List(MovMinus(lValue, rValue, t)), par, t)
    case (VAR_DECL, List(name: Name, GenericAst(scalarType: Vobla.ScalarType, _))) => VarDecl(name, scalarType, List(), t)
    case (ARRAY_TYPE, GenericAst(base: Vobla.ScalarType, _)::dims) => ArrayType(base.setAssignable(true), dims, t)
    case (VAR_DECL, List(name: Name, ArrayType(scalarType, dims, _))) => VarDecl(name, scalarType, dims, t)
    case (VAR_DECL, List(name: Name, typeOfBase)) => TypeOfDecl(name, typeOfBase, false, t)
    case (VAR_DECL_ASSIGN, List(name: Name, exp)) => TypeOfDecl(name, exp, true, t)
    case (FLOAT, List()) => GenericAst(Vobla.ValueVersion.FLOAT, t)
    case (DOUBLE, List()) => GenericAst(Vobla.ValueVersion.DOUBLE, t)
    case (COMPLEX_FLOAT, List()) => GenericAst(Vobla.ValueVersion.COMPLEX_FLOAT, t)
    case (COMPLEX_DOUBLE, List()) => GenericAst(Vobla.ValueVersion.COMPLEX_DOUBLE, t)
    case (REF, List(GenericAst(baseType: Vobla.ScalarType, _))) => GenericAst(baseType.setAssignableScalar(true), t)
    case (DECLI, List()) => GenericAst(Vobla.IndexType(), t)
    case (DECLV, List()) => GenericAst(Vobla.ValueType(), t)
    case (DECLC, List()) => GenericAst(Vobla.ComplexType(), t)
    case (DECLR, List()) => GenericAst(Vobla.RealType(), t)
    case (NONAME, List()) => GenericAst(None, t)
    case (COLON, List(e0, e1)) => Slice(e0, e1, t)
    case (MULT, List()) => GenericAst(Vobla.FullSlice(), t)
    case (EQ, List(e0, e1)) => Comparison(Vobla.ComparisonOp.EQ, e0, e1, t)
    case (NEQ, List(e0, e1)) => Comparison(Vobla.ComparisonOp.NEQ, e0, e1, t)
    case (MORE, List(e0, e1)) => Comparison(Vobla.ComparisonOp.MORE, e0, e1, t)
    case (LESS, List(e0, e1)) => Comparison(Vobla.ComparisonOp.LESS, e0, e1, t)
    case (LESSEQ, List(e0, e1)) => Comparison(Vobla.ComparisonOp.LESSEQ, e0, e1, t)
    case (MOREEQ, List(e0, e1)) => Comparison(Vobla.ComparisonOp.MOREEQ, e0, e1, t)
    case (LOR, List(e0, e1)) => Or(e0, e1, t)
    case (LAND, List(e0, e1)) => And(e0, e1, t)
    case (LNOT, List(e)) => Not(e, t)
    case (LEN, List(e)) => Len(e, None, t)
    case (LEN, List(e, i: IndexConstant)) => Len(e, Some(i), t)
    case (CALL, List(name: Name, ParserList(args, _))) => Call(name, args, t)
    case (ITERATION, List(ParserList(vars, _), e)) => Iteration(vars.map(getIterVar), e, t)
    case (CALL, List(GenericAst(name: BuiltinMethod.BuiltinMethod, _), ParserList(List(arg), _))) => BuiltinCall(name, arg, t)
    case (MAIN_DIAGONAL, List()) => GenericAst(BuiltinMethod.Diagonal, t)
    case (ANTI_DIAGONAL, List()) => GenericAst(BuiltinMethod.AntiDiagonal, t)
    case (TRANSPOSED, List()) => GenericAst(BuiltinMethod.Transposed, t)
    case (CONJUGATED, List()) => GenericAst(BuiltinMethod.Conjugated, t)
    case (REVERSED, List()) => GenericAst(BuiltinMethod.Reversed, t)
    case (COLUMN, List()) => GenericAst(BuiltinMethod.Column, t)
    case (ROW, List()) => GenericAst(BuiltinMethod.Row, t)
    case (RE, List()) => GenericAst(BuiltinMethod.Re, t)
    case (IM, List()) => GenericAst(BuiltinMethod.Im, t)
    case (RANGE, List()) => GenericAst(BuiltinMethod.Range, t)
    case (APPLY_CONJUGATED, List()) => GenericAst(BuiltinMethod.ApplyConjugated, t)
    case (RANGE, subTypes) => RangeType(subTypes, t)
    case ast => ice(ast, "invalid ast")
  }

  private def makeProgram(nodeData: Int, childs: List[Ast], t: Tree) = (nodeData, childs) match {
    case (PROGRAM, List(ParserList(imps, _), ParserList(decls, _))) if checkList[Import](imps) => Program(castList[Import](imps), decls, t)
    case ast => ice(ast, "invalid ast")
  }

  private def treeToScala[T <: Ast](op: (Int, List[Ast], Tree) => T)(tree: Tree): T = {
    val buffer = ArrayBuffer[Tree]()
    for(i <- 0 to tree.getChildCount - 1) buffer += tree.getChild(i)
    op(tree.getType, buffer.toList.map(treeToScala(makeAst)), tree)
  }

  def apply(antlrTree: program_return) = treeToScala(makeProgram)(antlrTree.getTree)
}
