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

import com.arm.carp.frontends.vobla.Ast._
import com.arm.carp.{ vobla => Vobla }
import com.arm.carp.vobla.ComparisonOp.ComparisonOp
import com.arm.carp.pencil.Assertable
import com.arm.carp.pencil.BuiltIn
import com.arm.carp.pencil.Common
import org.antlr.runtime.tree.Tree
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.collection.mutable.Stack

class Typer(
    val filename: String,
    private val functions: HashMap[String, Vobla.VoblaFunction],
    private val storages: HashMap[String, Vobla.Storage],
    private val interfaces: HashMap[(String, Int), Vobla.Interface],
    private val types: HashMap[String, Vobla.MethodImplementerVersion],
    private val views: HashMap[String, Vobla.VoblaView]) extends Common with Assertable {

  private case class Environment(varTable: Vobla.VarTable, returnType: Option[Vobla.Type] = None, functions: Map[String, Vobla.VoblaFunction] = Map(),
      selfMethods: Map[String, (Vobla.FunctionDecl, Vobla.ScalarType)] = Map(), memberVars: Set[Vobla.Expression] = Set(),
      isMemberSize: Boolean = false, isMethod: Boolean = false, isView: Boolean = false, isReturn: Boolean = false) {
  }

  private val globalNames = HashSet((Vobla.BuiltInInterfaces.interfaces.keys ++ functions.keys ++ storages.keys ++ types.keys ++ views.keys).toSeq :_* )
  // To keep track of global objects
  private val localNames = HashSet[Object]()
  private val exportNames = HashMap[String, () => Vobla.FunctionVersion]()
  private val intrinsics = BuiltIn.function

  private def addInterface(i: Vobla.Interface) = interfaces += (((i.name, i.dim), i))

  private def topLevelDef(ast: Ast) = ast match {
    case ast: Function => function(ast)
    case ast: Export => export(ast)
    case ast: Storage => storageDecl(ast)
    case ast: Interface => interfaceDecl(ast)
    case ast: Type => typeDecl(ast)
    case ast: TypeDef => typeDef(ast)
    case ast: Ast.View => viewDecl(ast)
    case _ => ice(ast, "toplevel definition expected")
  }

  private def function(fun: Function) = {
    val name = topLevelName(fun.name)
    val varTable = new Vobla.VarTable()
    lazy val returnType = voblaType(fun.returnType)
    lazy val init = fun.params.map(functionParameter(Environment(varTable))).flatten
    lazy val body = init ::: functionBody(fun.body, Environment(varTable, Some(returnType), functions.toMap))
    lazy val argsTypes = { init; varTable.locals.map(a => a.expType) }
    val function = Vobla.VoblaFunction(new Vobla.FunctionDecl(name, returnType, argsTypes), varTable, () => body)
    functions += ((name, function))
    function
  }

  private def voblaType(ast: Ast): Vobla.Type = ast match {
    case Void(_) => Vobla.VoidType()
    case GenericAst(t: Vobla.ScalarType, _) => t
    case RangeType(subTypes, _) => Vobla.RangeType(subTypes.map(voblaType))
    case InterfaceType(id, base, _) => Vobla.InterfaceType(getInterface(id), base)
  }

  private def export(export: Export) = {
    val function = getFunction(functions.toMap)(export.funName)
    val exportName = export.name.text
    if(exportNames.contains(exportName)) throw new RedefinedError(exportName, export.tree)
    val maker = () => {
      val vType = export.valueType
      val argTypesMap = export.params.foldLeft(Map[Vobla.Variable, Vobla.ArgVersion]()) { (m, p) =>
        val t @ (arg, argType) = argumentType(p, function.vars)
        if(m.contains(arg)) throw new RedefinedError(arg.name, p.tree)
        m + t
      }
      val argTypes = function.vars.locals.map{a => (a.expType, argTypesMap.get(a)) match {
        case (_, Some(version)) => version
        case (_: Vobla.ScalarType, None) => Vobla.ScalarVersion(false)
        case (Vobla.ArrayType(_, i), None) => Vobla.ArrayVersion(false, true, Vobla.ArrayPart.Full,
            List.tabulate(i, i)((i, j) => if(i == j) 1 else 0))
        case (_: Vobla.InterfaceType, None) => throw new UnspecifiedError(a.name, export.tree)
        case (t, _) => ice(t, "Invalid argument type")
      }}
      Vobla.FunctionVersion(vType, function, argTypes)
    }
    exportNames += ((exportName, maker))
  }

  val matrixFactors = List(List(1, 0), List(0, 1))

  object ViewQualifier extends Enumeration {
    type ViewQualifier = Value
    val Conjugated, Transposed, Reversed, Diagonal, AntiDiagonal, Column, Row, Array = Value
  }

  private def argumentType(argType: ArgumentType, table: Vobla.VarTable) = {
    // Get the argumentAny
    val variable = getVariable(Environment(table))(argType.name) match {
      case v: Vobla.Variable => v
      case e => ice(e, "Variable expected")
    }
    // Build the qualifiers set
    import this.ViewQualifier._;
    val qualifiers = HashSet[ViewQualifier]()
    var typeName: Option[TypeVersion] = None
    def isTypeAllowed = qualifiers.intersect(Set(Column, Row, Diagonal, AntiDiagonal)).isEmpty && typeName.isEmpty
    argType.qualifiers.map{
      case GenericAst(BuiltinMethod.Conjugated, _) if !qualifiers.contains(Conjugated) => qualifiers += Conjugated
      case GenericAst(BuiltinMethod.Transposed, _) if !qualifiers.contains(Transposed) => qualifiers += Transposed
      case GenericAst(BuiltinMethod.Reversed, _) if !qualifiers.contains(Reversed) => qualifiers += Reversed
      case GenericAst(BuiltinMethod.Column, _) if isTypeAllowed => qualifiers += Column
      case GenericAst(BuiltinMethod.Row, _) if isTypeAllowed => qualifiers += Row
      case GenericAst(BuiltinMethod.Diagonal, _) if isTypeAllowed => qualifiers += Diagonal
      case GenericAst(BuiltinMethod.AntiDiagonal, _) if isTypeAllowed => qualifiers += AntiDiagonal
      case TypeVersion(name, None, _) if name.text == "Array" && isTypeAllowed => qualifiers += Array
      case t: TypeVersion if isTypeAllowed => typeName = Some(t)
      case ast => throw new TypingError("Invalid qualifier", ast.tree)
    }
    // Build the version
    val isConjugated = qualifiers.contains(Conjugated)
    val version = (variable.expType, typeName) match {
      case (_: Vobla.ScalarType, None) =>
        if(!qualifiers.intersect(Set(Transposed, Reversed, Column, Row, Diagonal, AntiDiagonal, Array)).isEmpty)
          throw new TypingError("Invalid qualifiers", argType.tree)
        Vobla.ScalarVersion(isConjugated)
      case (Vobla.ArrayType(_, 1), None) => getArrayVersion(isConjugated, qualifiers.toSet, argType.tree)
      case (Vobla.ArrayType(_, 2), None) => getMatrixVersion(isConjugated, qualifiers.toSet, argType.tree)
      case (Vobla.InterfaceType(interface, _), None) =>
        if(!Vobla.BuiltInInterfaces.array(interface.dim).allInterfaces.contains(interface) || interface.dim > 2)
          throw new TypingError("Invalid qualifier", argType.tree)
        (if(interface.dim == 1) getArrayVersion else getMatrixVersion)(isConjugated, qualifiers.toSet, argType.tree)
      case (expectedType: Vobla.InterfaceType, Some(typeVersion)) =>
        if(!qualifiers.intersect(Set(Transposed, Reversed)).isEmpty)
          throw new TypingError("Invalid qualifiers", argType.tree)
        val t = getTypeVersion(typeVersion)
        val interType = Vobla.InterfaceType(t.base, variable.expType.base)
        if(!(interType > expectedType)) throw TypeError(interType, typeVersion.tree)
        Vobla.InterfaceVersion(t, isConjugated, true)
      case (_, Some(typeName)) => throw new TypingError("Invalid qualifier", typeName.tree)
      case (_, _) => throw new TypingError("Qualifier expected", argType.tree)
    }
    (variable, version)
  }

  private def getTypeVersion(version: TypeVersion): Vobla.MethodImplementerVersion = version.subVersion match {
    case None => getType(version.name)
    case Some(subVersion) => Vobla.ViewVersion(getView(version.name), getTypeVersion(subVersion))
  }


  private def typeDef(typeDef: TypeDef) = {
    val name = topLevelName(typeDef.name)
    val version = getTypeVersion(typeDef.version)
    types += name -> version
  }

  private val getArrayVersion = (isConjugated: Boolean, qualifiers: Set[ViewQualifier.ViewQualifier], tree: Tree) => {
    import this.ViewQualifier._;
    if(qualifiers.contains(Transposed)) throw new TypingError("Invalid qualifiers", tree)
    val unreversedFactors = qualifiers.intersect(Set(Column, Row, Diagonal, AntiDiagonal)).toList match {
      case List() => List(List(1))
      case List(Row) => List(List(0, 1))
      case List(Column) => List(List(1, 0))
      case List(Diagonal) => List(List(1, 1))
      case List(AntiDiagonal) => List(List(1, -1))
    }
    val factors = if(qualifiers.contains(Reversed)) unreversedFactors.map(l => l.map(i => -i)) else unreversedFactors
    Vobla.ArrayVersion(isConjugated, true, Vobla.ArrayPart.Full, factors)
  }

  private val getMatrixVersion = (isConjugated: Boolean, qualifiers: Set[ViewQualifier.ViewQualifier], tree: Tree) => {
    import this.ViewQualifier._;
    if(!qualifiers.intersect(Set(Reversed, Column, Row, Diagonal, AntiDiagonal)).isEmpty)
          throw new TypingError("Invalid qualifiers", tree)
    val isTransposed = qualifiers.contains(Transposed)
    Vobla.ArrayVersion(isConjugated, true, Vobla.ArrayPart.Full, if(!isTransposed) matrixFactors else matrixFactors.reverse, isTransposed)
  }

  private def storageDecl(storage: Storage) = {
    val env = Environment(new Vobla.VarTable())
    val sizes = storage.members.map(member(env)).filter{case (v, s) => !s.isEmpty}.toMap
    val name = topLevelName(storage.name)
    storages += ((name, new Vobla.Storage(name, env.varTable, sizes)))
  }

  private def member(env: Environment)(member: Member) = {
    val voblaType = if(member.dims.isEmpty) member.mType else Vobla.ArrayType(member.mType.setAssignable(true), member.dims.size)
    val dims = member.dims.map(getIndex(env.copy(isMemberSize = true)))
    (addVar(env)(member.name, voblaType), dims)
  }

  private def interfaceDecl(interface: Interface) = {
    val name = interface.name.text
    if(interfaces.contains((interface.name.text, interface.dim)) || functions.contains(name) || storages.contains(name) || types.contains(name))
      throw new RedefinedError(name, interface.name.tree)
    globalNames += name
    localNames += ((name, interface.dim))
    lazy val (subInterfaces, subMethods) = implements(interface.implements, interface.tree)
    lazy val properMethods = interface.methods.foldLeft((subMethods, Set[Vobla.FunctionDecl]())){ case ((m, pm), mAst) =>
      val meth = methodHeader(Environment(new Vobla.VarTable()))(mAst)
      if(m.contains(meth.name)) throw RedefinedError(meth.name, mAst.tree)
      (m + ((meth.name, (meth, Vobla.ValueType(true)))), pm + meth)
    }._2
    lazy val checkedSubInterfaces = {
      if(interface.dim > 0 && !subInterfaces.contains(Vobla.BuiltInInterfaces.measurable(interface.dim)))
        throw new TypingError("Missing measurable interface", interface.tree)
      subInterfaces
    }
    addInterface(new Vobla.Interface(name, checkedSubInterfaces, properMethods, interface.dim))
  }

  private def typeDecl(t: Type) = {
    val name = topLevelName(t.name)
    lazy val storage = getStorage(t.storage)
    lazy val (interfaces, methodDecls) = implements(t.implements, t.tree)
    lazy val env = Environment(storage.members.branch)
    lazy val methods = methodCreator(t.methods, env.copy(memberVars = storage.members.allVariables.toSet) , methodDecls, t.tree)_
    types += name -> Vobla.TypeVersion(new Vobla.VoblaType(name, storage, t.members.map(member(env)(_)._1), interfaces, methods))
  }

  private def viewDecl(view: Ast.View) = {
    val name = topLevelName(view.name)
    lazy val interface = getInterface(view.interface)
    lazy val (interfaces, methodDecls) = implements(view.implements, view.tree)
    lazy val baseVar = Vobla.Variable("base", Vobla.InterfaceType(interface, Vobla.ValueType(true)))
    lazy val env = {
      val table = new Vobla.VarTable(None)
      table.add(baseVar)
      Environment(table, isView = true)
    }
    lazy val methods = methodCreator(view.methods, env.copy(memberVars = env.varTable.allVariables.toSet), methodDecls, view.tree)_
    views += name -> new Vobla.VoblaView(name, interface, baseVar, interfaces, methods)
  }

  private def methodCreator(methodsAst: List[Method], env: Environment,
      methodDecls: Map[String, (Vobla.FunctionDecl, Vobla.ScalarType)], t: Tree)(self: Vobla.MethodImplementer) = {
    env.varTable.add(self.thisVar)
    val methods = methodsAst.foldLeft((Map[Vobla.FunctionDecl, Vobla.Method]())) { (map, method) =>
      val methodEnv = env.copy(varTable = env.varTable.branch, memberVars = env.memberVars + self.thisVar, isMethod = true)
      val decl = methodHeader(methodEnv)(method.decl)
      def throwError(msg: String) = throw new TypingError(decl.name + " " + msg, method.decl.tree)
      val (previousDecl, specType) = methodDecls.get(decl.name).getOrElse(throwError(" is undeclared"))
      if (map.contains(previousDecl)) throwError("has been implemented twice")
      if (!(decl.name == previousDecl.name && decl.returnType == previousDecl.returnType.specialize(specType)
        && decl.args == previousDecl.args.map(_.specialize(specType))))
        throw new TypingError("Method definition doesn't match declaration ("
            + decl + " vs " + previousDecl + " spec as " + specType + ")", method.decl.tree)
      val selfMethods = methodDecls.filterKeys(map.keySet.map(_.name))
      map + ((previousDecl, methodImpl(decl, previousDecl, method, methodEnv.copy(selfMethods = selfMethods))))
    }
    val undef = methodDecls -- methods.keys.map(_.name)
    undef.foreach { case (name, _) => throw UndefinedError(name, t) }
    methods
  }

  private def methodImpl(decl: Vobla.FunctionDecl, previousDecl: Vobla.FunctionDecl, method: Method, env: Environment) = {
    decl.returnType match {
      case t: Vobla.RangeType =>
        val bodyEnv = env.copy(varTable = env.varTable.branch, returnType = Some(t))
        val body = functionBody(method.body, bodyEnv)
        Vobla.RangeImpl(previousDecl, env.varTable.locals, bodyEnv.varTable.subVariables, body)
      case _ =>
        if(method.body.isEmpty) throw new TypingError("Method must have at least one statement", method.tree)
        val varTable = env.varTable.branch
        val body = method.body.init.flatMap(operation(_, env.copy(varTable = varTable)))
        val returnOp = method.body.last match {
          case op: Return => returnOperation(env.copy(varTable = varTable, returnType = Some(decl.returnType), isReturn=true))(op)
          case _ => throw new TypingError("Last statement must be return", method.tree)
        }
        Vobla.MethodImpl(previousDecl, env.varTable.locals, varTable.subVariables, body, returnOp)
    }
  }

  val currentImplements = HashSet[Tree]()
  private def implements(names: List[InterfaceType], t: Tree) = {
    if(!currentImplements.add(t))
      throw new TypingError("Interface " + t.getText + " implements itself", t)
    val properSubInterfaces = checkUnicity[InterfaceType, Vobla.Interface, Vobla.ScalarType](names, i => (getInterface(i.interface), i.base))
    val subInterfaces = properSubInterfaces.flatMap{ case elem @ (i, base) => elem :: i.subInterfaces.toList.map{ case (i, b) => (i, b.specialize(base))
      }}.groupBy(_._1).mapValues{ x => x.unzip._2.reduceLeft{ (a, b) =>
        if(a.setAssignable(b.assignable) != b) throw new TypingError("Incompatibles value type for interface " + x(0)._1, t)
          a.setAssignable(a.assignable || b.assignable)
      }}
    val methods = subInterfaces.foldLeft(Map[String, (Vobla.FunctionDecl, Vobla.ScalarType)]()){ case (m, (i, assignable)) =>
      i.properMethods.foldLeft(m){ case (m, item) =>
        if(m.contains(item.name)) throw new TypingError("method " + item.name + " has aleready been defined in another interface", t)
        m + ((item.name, (item, assignable)))
      }
    }
    currentImplements.remove(t)
    (subInterfaces, methods)
  }

  private def methodHeader(env: Environment)(hdr: MethodHdr) =
    new Vobla.FunctionDecl(hdr.name.text, voblaType(hdr.returnType), hdr.arguments.map{ arg => addVar(env)(arg.name, arg.t); arg.t})

  private def functionParameter(env: Environment)(ast: Ast) = ast match {
    case ScalarParam(name, t, _) => addVar(env)(name, t); List()
    case ast: ArrayParam => arrayParam(env)(ast)
    case InterfaceParam(name, assignable, iName, sType, dims, _) =>
      val interface = getInterface(InterfaceId(iName, dims.size, iName.tree))
      val t = Vobla.InterfaceType(interface, sType.setAssignable(assignable))
      handleParamSize(env)(addVar(env)(name, t), dims, ast.tree)
    case _ => ice(ast, "Function parameter expected")
  }

  private def handleParamSize(env: Environment)(v: Vobla.Variable, dims: List[Ast], tree: Tree) = dims.zipWithIndex.flatMap{
      case (name: Name, i) =>
        val size = Vobla.MethodCallExpression(v, Vobla.BuiltInInterfaces.GetLen(i+1), List(), Vobla.ValueType(true))
        if(env.varTable.addExpr(name.text, size)) None
        else Some(Vobla.AssumeOperation(Vobla.Comparison(Vobla.ComparisonOp.EQ, size, getIndex(env)(name))))
      case (Void(_), _) => None
      case (IndexConstant(size, _), i) =>
        val m = Vobla.MethodCallExpression(v, Vobla.BuiltInInterfaces.GetLen(i+1), List(), Vobla.ValueType(true))
        Some(Vobla.AssumeOperation(Vobla.Comparison(Vobla.ComparisonOp.EQ, Vobla.IndexConstant(size), m)))
      case _ => ice(tree, "int constant or variable expected")
    }

  private def arrayParam(env: Environment)(param: ArrayParam) = {
    val baseType = param.baseType.setAssignable(param.assignable)
    val v = param.dims match {
      case List(dim) =>
        val t = Vobla.ArrayType(baseType, 1)
        addVar(env)(param.name, t)
      case List(dim0, dim1) =>
        val t = Vobla.ArrayType(baseType, 2)
        addVar(env)(param.name, t)
    }
    handleParamSize(env)(v, param.dims, param.tree)
  }

  private def functionBody(ops: List[Ast], env: Environment): List[Vobla.Operation] = {
    val subEnv = env.copy(varTable = env.varTable.branch)
    ops.flatMap(operation(_, subEnv))
  }


  private def returnOperation(env: Environment)(r: Return) = {
    (r.value.map(expression(env)), env.returnType) match {
      case (_, None) => throw new  TypingError("Return is forbiden here", r.tree)
      case (None, Some(Vobla.VoidType())) => Vobla.ReturnOperation(None, Vobla.VoidType())
      case (Some(exp), Some(returnType)) if exp.expType > returnType => Vobla.ReturnOperation(Some(exp), returnType)
      case (Some(exp), _) => throw TypeError(exp.expType, r.tree)
      case (None, _) => throw new TypingError("return type is not void", r.tree)
    }
  }

  private def operation(ast: Ast, env: Environment) = ast match {
    case Yield(expsAst, tree, invConst) =>
      val args = expsAst.map(expression(env.copy(functions = Map(), isReturn = true)))
      val t = Vobla.RangeType(args.map(_.expType))
      env.returnType match {
        case None => throw new TypingError("Return is forbiden here", tree)
        case Some(rT) if t > rT => Some(Vobla.YieldOperation(args))
        case _ => throw TypeError(t, tree)
      }
    case r: Return => Some(returnOperation(env)(r))
    case While(cond, body, _) => Some(Vobla.WhileOperation(boolean(env)(cond), functionBody(body, env)))
    case If(cond, trueBody, falseBody, _) =>
      Some(Vobla.IfOperation(boolean(env)(cond), functionBody(trueBody, env), functionBody(falseBody, env)))
    case For(itersAst, body, parallel, _) =>
      val subEnv = env.copy(varTable = env.varTable.branch)
      val (iters, members) = itersAst.map(iteration(subEnv)).unzip
      Some(Vobla.ForOperation(iters, functionBody(body, subEnv.copy(memberVars = subEnv.memberVars ++ members.flatten.toSet)), parallel))
    case TypeOfDecl(name, init, assign, t) =>
      val exp = expression(env)(init)
      val v = addVar(env)(name, exp.expType.setAssignable(true))
      exp.expType match {
        case _: Vobla.ScalarType if assign => Some(movOperation(env)(v, exp, t))
        case _: Vobla.ScalarType => None
        case _: Vobla.ArrayType => Some(Vobla.TypeofArrayDecl(v, exp, assign))
        case t => throw new TypeError(t, init.tree)
      }
    case VarDecl(name, baseType, dims, _) =>
      val voblaType = if(dims.isEmpty) baseType.setAssignable(true)  else Vobla.ArrayType(baseType.setAssignable(true), dims.size)
      val v = addVar(env)(name, voblaType)
      if(dims.isEmpty) None else Some(Vobla.ArrayDecl(v, baseType, dims.map(getIndex(env))));
    case op: MovOp =>
      val lValue = expression(env)(op.lValue)
      val rValue = expression(env)(op.rValue)
      if(!(rValue.expType.base > lValue.expType.base.setAssignable(false)))
        throw new TypingError("Invalid assignment operation: <" + lValue.expType + "> = <" + rValue.expType + ">", op.tree)
    if(!lValue.expType.assignable)
      throw new TypingError("Cannot assign to non-assignable argument " + lValue + " of type " + lValue.expType, op.tree)
      Some((op match {
        case _: Mov => movOperation
        case _: MovPlus => movPlusOperation
        case _: MovMinus => movMinusOperation
        case _: MovMul => movMulOperation
        case _: MovDiv => movDivOperation
    })(env)(lValue, rValue, op.tree))
    case ast: Call => expression(env)(ast) match {
      case e: Vobla.CallExpression if e.expType == Vobla.VoidType() => Some(Vobla.CallOperation(e))
      case _ => throw new TypingError("Invalid call operation", ast.tree)
    }
    case _ => ice(ast, "Operation expected")
  }

  private def isAccessible(t: Vobla.InterfaceType) = t.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.accessible(t.dim))
  private def isIterable(t: Vobla.InterfaceType) = t.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.iterable(t.dim))
  private def isLIterable(t: Vobla.InterfaceType) = t.inter.allInterfaces.get(Vobla.BuiltInInterfaces.iterable(t.dim)).exists(_.assignable)
  private def isSparseIterable(t: Vobla.InterfaceType) = t.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.sparseIterable(t.dim))
  private def isLSparseIterable(t: Vobla.InterfaceType) = t.inter.allInterfaces.get(Vobla.BuiltInInterfaces.sparseIterable(t.dim)).exists(_.assignable)

  private val movOperation = (env: Environment) => (lValue: Vobla.Expression, rValue: Vobla.Expression, tree: Tree) => {
    (lValue.expType, rValue.expType) match {
      case (t0:Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MovOperation(lValue, rValue, false, false, false)
      case (t0_, t1: Vobla.ScalarType) =>
        val t0 = getInterface(env)(t0_, tree)
        if(isLIterable(t0))
          Vobla.MovOperation(lValue, rValue, false, true, false)
        else
          throw TypeError(t1, tree)
      case (t0_, t1_) =>
        val t0 = getInterface(env)(t0_, tree)
        val t1 = getInterface(env)(t1_, tree)
        if(t0.dim != t1.dim) throw TypeError(t1, tree)
        if(isAccessible(t0) && isIterable(t1))
          Vobla.MovOperation(lValue, rValue, false, false, false)
        else if(isLIterable(t0) && isAccessible(t1))
          Vobla.MovOperation(lValue, rValue, false, true, false)
        else
          throw TypeError(t1, tree)
    }
  }

  private val movPlusOperation = (env: Environment) => (lValue: Vobla.Expression, rValue: Vobla.Expression, tree: Tree) => {
    (lValue.expType, rValue.expType) match {
      case (t0:Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MovPlusOperation(lValue, rValue, false, false)
      case (t0_, t1: Vobla.ScalarType) =>
        val t0 = getInterface(env)(t0_, tree)
        if(isLIterable(t0))
          Vobla.MovPlusOperation(lValue, rValue, true, false)
        else
          throw TypeError(t1, tree)
      case (t0_, t1_) =>
        val t0 = getInterface(env)(t0_, tree)
        val t1 = getInterface(env)(t1_, tree)
        if(t0.dim != t1.dim) throw TypeError(t1, tree)
        if(isAccessible(t0) && isSparseIterable(t1))
          Vobla.MovPlusOperation(lValue, rValue, false, true)
        else if(isLIterable(t0) && isAccessible(t1))
          Vobla.MovPlusOperation(lValue, rValue, true, false)
        else
          throw TypeError(t1, tree)
    }
  }

  private val movMinusOperation = (env: Environment) => (lValue: Vobla.Expression, rValue: Vobla.Expression, tree: Tree) => {
    (lValue.expType, rValue.expType) match {
      case (t0:Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MovMinusOperation(lValue, rValue, false, false)
      case (t0_, t1: Vobla.ScalarType) =>
        val t0 = getInterface(env)(t0_, tree)
        if(isLIterable(t0))
          Vobla.MovMinusOperation(lValue, rValue, true, false)
        else
          throw TypeError(t1, tree)
      case (t0_, t1_) =>
        val t0 = getInterface(env)(t0_, tree)
        val t1 = getInterface(env)(t1_, tree)
        if(t0.dim != t1.dim) throw TypeError(t1, tree)
        if(isAccessible(t0) && isSparseIterable(t1))
          Vobla.MovMinusOperation(lValue, rValue, false, true)
        else if(isLIterable(t0) && isAccessible(t1))
          Vobla.MovMinusOperation(lValue, rValue, true, false)
        else
          throw TypeError(t1, tree)
    }
  }

  private val movMulOperation = (env: Environment) => (lValue: Vobla.Expression, rValue: Vobla.Expression, tree: Tree) => {
    (lValue.expType, rValue.expType) match {
      case (t0:Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MovMulOperation(lValue, rValue, false, false)
      case (t0_, t1: Vobla.ScalarType) =>
        val t0 = getInterface(env)(t0_, tree)
        if(isLSparseIterable(t0))
          Vobla.MovMulOperation(lValue, rValue, true, true)
        else
          throw TypeError(t1, tree)
      case (t0_, t1_) =>
        val t0 = getInterface(env)(t0_, tree)
        val t1 = getInterface(env)(t1_, tree)
        if(t0.dim != t1.dim) throw TypeError(t1, tree)
        if(isLSparseIterable(t0) && isAccessible(t1))
          Vobla.MovMulOperation(lValue, rValue, true, true)
        else if(isAccessible(t0) && isIterable(t1))
          Vobla.MovMulOperation(lValue, rValue, false, false)
        else
          throw TypeError(t1, tree)
    }
  }

  private val movDivOperation = (env: Environment) => (lValue: Vobla.Expression, rValue: Vobla.Expression, tree: Tree) => {
    (lValue.expType, rValue.expType) match {
      case (t0:Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MovDivOperation(lValue, rValue, false, false)
      case (t0_, t1: Vobla.ScalarType) =>
        val t0 = getInterface(env)(t0_, tree)
        if(isLSparseIterable(t0))
          Vobla.MovDivOperation(lValue, rValue, true, true)
        else
          throw TypeError(t1, tree)
      case (t0_, t1_) =>
        val t0 = getInterface(env)(t0_, tree)
        val t1 = getInterface(env)(t1_, tree)
        if(t0.dim != t1.dim) throw TypeError(t1, tree)
        if(isLSparseIterable(t0) && isAccessible(t1))
          Vobla.MovDivOperation(lValue, rValue, true, true)
        else if(isAccessible(t0) && isIterable(t1))
          Vobla.MovDivOperation(lValue, rValue, false, false)
        else
          throw TypeError(t1, tree)
    }
  }

  private def boolean(env: Environment)(ast: Ast): Vobla.Condition = ast match {
    case Or(e0, e1, _) => Vobla.LogicalOr(boolean(env)(e0), boolean(env)(e1))
    case And(e0, e1, _) => Vobla.LogicalAnd(boolean(env)(e0), boolean(env)(e1))
    case Not(e, _) => Vobla.LogicalNot(boolean(env)(e))
    case Comparison(op, e0, e1, _) => Vobla.Comparison(op, getScalar(env)(e0), getScalar(env)(e1))
    case _ => ice(ast, "boolean expression expected")
  }

  private def iteration(env: Environment)(iter: Iteration) = {
    val (range, rangeType) = getRange(env)(iter.exp)
    val (_, returnRangeType) = getRange(env.copy(isReturn = true))(iter.exp)
    if(iter.vars.size != rangeType.dimension) throw new TypingError("Invalid dimension", iter.tree)
    val (vars, members) = (iter.vars, rangeType.subTypes, returnRangeType.subTypes).zipped.map{(x, t, tr) =>  x.map{s =>
      val v = addVar(env)(s, t.setAssignable(tr.assignable || t.assignable))
      (Some(v), Some(v).filter(_ => !t.assignable && tr.assignable))
    }}.map(_.getOrElse(None, None)).unzip
    (Vobla.Iteration(vars, range), members.flatten)
  }

  private def getBiggestAccess(i: Vobla.InterfaceType, tree: Tree) = {
    if(i.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.accessible(i.dim)))
      Vobla.BuiltInInterfaces.accessible(i.dim)
    else if(i.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.iterable(i.dim)))
      Vobla.BuiltInInterfaces.iterable(i.dim)
    else if(i.inter.allInterfaces.contains(Vobla.BuiltInInterfaces.sparseIterable(i.dim)))
      Vobla.BuiltInInterfaces.sparseIterable(i.dim)
    else
      throw new TypeError(i, tree)
  }

  private def expression(env: Environment)(ast: Ast): Vobla.Expression = ast match {
    case MultExpression(e0, e1, _) =>
      val exp0 = expression(env)(e0)
      val exp1 = expression(env)(e1)
      (exp0.expType, exp1.expType) match {
        case (t0: Vobla.ScalarType, t1: Vobla.ScalarType) => Vobla.MultExpression(exp0, exp1, Vobla.Type.calculateType(t0, t1))
        case (t0: Vobla.ScalarType, t1_) =>
          val t1 = getInterface(env)(t1_, e1.tree)
          val inter = getBiggestAccess(t1, e1.tree)
          Vobla.ObjScalarMultExpression(exp0, exp1, Vobla.InterfaceType(inter, Vobla.Type.calculateType(t0, t1.subInterfaceBase(inter))))
        case (t0_, t1: Vobla.ScalarType) =>
          val t0 = getInterface(env)(t0_, e1.tree)
          val inter = getBiggestAccess(t0, e0.tree)
          Vobla.ObjScalarMultExpression(exp1, exp0, Vobla.InterfaceType(inter, Vobla.Type.calculateType(t1, t0.subInterfaceBase(inter))))
        // TODO: Array/Array mult
        case (_, t1) => throw TypeError(t1, e1.tree)
      }
    case Call(name, argsAst, t) if !env.isMemberSize =>
      if(intrinsics.contains(name.text)) {
         val args = argsAst.map(checkExpType(env)(t => t.isInstanceOf[Vobla.IndexType] || t.isInstanceOf[Vobla.RealType], _))
         if(!(args.size == intrinsics(name.text))) throw new TypingError("Bad number of arguments", t)
        Vobla.IntrinsicCallExpression(name.text, args)
      }
      else if(env.isMethod && !env.isView && types.contains(name.text)) {
        val t = getType(name) match {
          case Vobla.TypeVersion(t) => t
          case t => throw new TypingError("Layout name expected" , name.tree)
        }
        val args = argsAst.map(expression(env.copy(isReturn = false)))
        if(t.members.size != args.size || !(t.members, args).zipped.forall((v, a) => a.expType > v.expType))
          throw new TypingError("Invalid arguments", name.tree)
        Vobla.ConstructorExpression(t, args)
      }
      else if(env.isMethod && views.contains(name.text) && argsAst.size == 1) {
        val view = getView(name)
        val arg = expression(env.copy(isReturn = false))(argsAst.head)
        val specType = arg.expType match {
          case Vobla.InterfaceType(iBase, base) if iBase == view.interface => base
          case Vobla.InterfaceType(iBase, base) =>
            iBase.subInterfaces.get(view.interface).getOrElse(throw new TypeError(arg.expType, argsAst.head.tree)).specialize(base)
          case _ => throw new TypeError(arg.expType, argsAst.head.tree)
        }
        new Vobla.ViewConstructorExpression(view, arg, Vobla.InterfaceType(view, specType))
      }
      else if(env.selfMethods.contains(name.text)) {
        val args = argsAst.map(expression(env.copy(isReturn = false)))
        val (method, specType) = env.selfMethods(name.text)
        if(!method.checkCall(args, specType)) throw new TypingError("Invalid arguments", t)
        new Vobla.BaseMethodCallExpression(method, args, specType)
      }
      else {
        val args = argsAst.map(expression(env.copy(isReturn = false)))
        val fun = getFunction(env.functions)(name)
        val specType = fun.decl.getSpecializedType(args)
        if(!fun.decl.checkCall(args, specType)) throw new TypingError("Invalid arguments", t)
        Vobla.CallExpression(fun, args, specType)
      }
    case ast: BuiltinCall if !env.isMemberSize => callExpression(env)(ast)
    case ast: MethodCallExpression  if !env.isMemberSize => methodCallExpression(env)(ast)
    case SumExpression(iterations, base, t)  if !env.isMemberSize =>
      val subEnv = env.copy(varTable = env.varTable.branch)
      val (iters, _) = iterations.reverse.map(iteration(subEnv)).unzip
      if(iters.size != 1) throw new TypingError("sum can only iterate over one dimension", t)
      Vobla.SumExpression(iters.head, getScalar(subEnv)(base))
    case Len(e, None, t)  if !env.isMemberSize =>
      val base = expression(env)(e)
      val specType = base.expType match {
        case t: Vobla.ArrayType if t.dim == 1  => t.base
        case t: Vobla.InterfaceType if t.inter.dim == 1 => t.base
        case t => throw new TypeError(t, e.tree)
      }
      Vobla.MethodCallExpression(base, Vobla.BuiltInInterfaces.GetLen(1), List[Vobla.Expression](), specType)
    case Len(e, Some(i), _)  if !env.isMemberSize =>
      val base = expression(env)(e)
      val specType = base.expType match {
        case t: Vobla.ArrayType if t.dim >= i.i  => t.base
        case t: Vobla.InterfaceType if t.inter.dim >= i.i => t.base
        case t => throw new TypeError(t, e.tree)
      }
      Vobla.MethodCallExpression(base, Vobla.BuiltInInterfaces.GetLen(i.i), List(), specType)
    case UMinusExpression(e, _) =>
      val exp = expression(env)(e)
      exp.expType match {
        case t: Vobla.ScalarType => Vobla.UMinusExpression(exp, t.setAssignable(false))
        case t_ =>
          val t = getInterface(env)(t_, e.tree)
          val inter = getBiggestAccess(t, e.tree)
          Vobla.UMinusExpression(exp, Vobla.InterfaceType(inter, t.subInterfaceBase(inter).setAssignable(false)))
      }
    case ast: OffsetExpression if !env.isMemberSize => offset(env)(ast)
    case ast: ArrayConstant  if !env.isMemberSize => arrayCst(ast)
    case ast: Name => getVariable(env)(ast)
    case IndexConstant(i, _) => Vobla.IndexConstant(i)
    case RealConstant(r, _)  if !env.isMemberSize => Vobla.RealConstant(r)
    case SliceRange(low, up, by, t)  if !env.isMemberSize =>
      if(by == 0) throw new TypingError("loop step must be != 0", t)
      Vobla.SliceRange(getIndex(env)(low), getIndex(env)(up), by)
    case DivExpression(e0, e1, tree) =>
      val exp0 = expression(env)(e0)
      val exp1 = getScalar(env)(e1)
      exp0.expType match {
        case t0: Vobla.ScalarType => Vobla.DivExpression(exp0, exp1, Vobla.Type.calculateType(t0, exp1.expType.base))
        case t0_ =>
          val t0 = getInterface(env)(t0_, e0.tree)
          val inter = getBiggestAccess(t0, e0.tree)
          Vobla.DivExpression(exp0, exp1, Vobla.InterfaceType(inter, Vobla.Type.calculateType(t0.subInterfaceBase(inter), exp1.expType.base)))
      }
    case op: Op2ExpressionAst =>
      val exp0 = expression(env)(op.exp0)
      val exp1 = expression(env)(op.exp1)
      val (t, left) = (exp0.expType, exp1.expType) match {
        case (t0: Vobla.ScalarType, t1: Vobla.ScalarType) => (Vobla.Type.calculateType(t0, t1), false)
        case (t0_, t1: Vobla.ScalarType) =>
          val t0 = getInterface(env)(t0_, op.exp0.tree)
          if(!isIterable(t0)) throw new TypeError(t0, op.exp0.tree)
          val inter = getBiggestAccess(t0, op.exp0.tree)
          (Vobla.InterfaceType(inter, Vobla.Type.calculateType(t0.subInterfaceBase(inter), t1)), true)
        case (t0: Vobla.ScalarType, t1_) =>
          val t1 = getInterface(env)(t1_, op.exp1.tree)
          if(!isIterable(t1)) throw new TypeError(t1, op.exp1.tree)
          val inter = getBiggestAccess(t1, op.exp1.tree)
          (Vobla.InterfaceType(inter, Vobla.Type.calculateType(t1.subInterfaceBase(inter), t0)), false)
        case (t0_, t1_) =>
          val t0 = getInterface(env)(t0_, op.exp0.tree)
          val t1 = getInterface(env)(t1_, op.exp1.tree)
          if(t0.dim != t1.dim) throw TypeError(t0, op.exp0.tree)
          val accessible = Vobla.BuiltInInterfaces.accessible(t0.dim)
          if(isIterable(t0) && isAccessible(t1)) {
            val inter = getBiggestAccess(t0, op.exp0.tree)
            (Vobla.InterfaceType(inter, Vobla.Type.calculateType(t0.subInterfaceBase(inter), t1.subInterfaceBase(accessible))), true)
          } else if(isAccessible(t0) && isIterable(t1)) {
            val inter = getBiggestAccess(t1, op.exp1.tree)
            (Vobla.InterfaceType(inter, Vobla.Type.calculateType(t1.subInterfaceBase(inter), t0.subInterfaceBase(accessible))), false)
          } else throw TypeError(t0, op.exp0.tree)
      }
      op match {
        case _: PlusExpression => Vobla.PlusExpression(exp0, exp1, t, left)
        case _: MinusExpression => Vobla.MinusExpression(exp0, exp1, t, left)
      }
    case _ => throw new TypingError("Invalid expression", ast.tree)
  }

  private def callExpression(env: Environment)(call: BuiltinCall) = call.name match {
    case BuiltinMethod.Diagonal =>
      val matrix = getMatrix(env, call.arg)
      val t = matrix.expType.asInstanceOf[Vobla.ArrayType]
      Vobla.DiagExpression(matrix, Vobla.ArrayType(t.base, 1))
    case BuiltinMethod.AntiDiagonal =>
      val matrix = getMatrix(env, call.arg)
      val t = matrix.expType.asInstanceOf[Vobla.ArrayType]
      Vobla.AntiDiagExpression(matrix, Vobla.ArrayType(t.base, 1))
    case BuiltinMethod.Transposed =>
      Vobla.TransposedExpression(getMatrix(env, call.arg))
    case BuiltinMethod.Conjugated =>
      Vobla.ConjExpression(expression(env)(call.arg))
    case BuiltinMethod.Reversed => Vobla.ReversedExpression(getVector(env)(call.arg)._1)
    case BuiltinMethod.Re => Vobla.ReExpression(getScalar(env)(call.arg))
    case BuiltinMethod.Im => Vobla.ImExpression(getScalar(env)(call.arg))
    case BuiltinMethod.ApplyConjugated => Vobla.ApplyConjugatedExpression(expression(env)(call.arg))
  }

  private def methodCallExpression(env: Environment)(m : MethodCallExpression) = {
      val obj = expression(env)(m.base)
      val args = m.args.map(expression(env))
      val t = obj.expType match {
        case t: Vobla.InterfaceType => t
        case Vobla.ArrayType(base, dim) => Vobla.InterfaceType(Vobla.BuiltInInterfaces.array(dim), base)
        case t => throw TypeError(t, m.tree)
      }
      val (meth, specType) = getIdentifier(m.method, (s: Ast.Name) => t.inter.methodsDeclByName.get(s.text))
      if(!meth.checkCall(args, specType.specialize(t.base)))
        throw new TypingError("Invalid arguments type", m.method.tree)
      Vobla.MethodCallExpression(obj, meth, args, specType.specialize(t.base))
  }

  private def offset(env: Environment)(offset: OffsetExpression) = {
    val base = expression(env)(offset.base)
    base.expType match {
      case t: Vobla.ArrayType if t.dim != offset.offsets.size  => throw new TypingError("Incomplete offset expression", offset.tree)
      case t: Vobla.InterfaceType if !t.inter.methodDecls.contains(Vobla.BuiltInInterfaces.Access(offset.offsets.size)) =>
        throw new TypeError(t, offset.tree)
      case _: Vobla.ArrayType =>
        val offsets = offset.offsets.map{
          case Slice(e0, e1, _) => Vobla.Slice(getIndex(env)(e0), getIndex(env)(e1))
          case GenericAst(Vobla.FullSlice(), _) => Vobla.FullSlice()
          case exp => Vobla.Idx(getIndex(env)(exp))
        }
        val t = offsets.foldLeft(base.expType){
          case (t: Vobla.ArrayType, Vobla.FullSlice()) => t
          case (Vobla.ArrayType(base, 1), Vobla.Idx(_)) => base
          case (Vobla.ArrayType(base, d), Vobla.Idx(_)) => Vobla.ArrayType(base, d-1)
          case (t: Vobla.ArrayType, Vobla.Slice(low, up)) => t
        }
        Vobla.OffsetExpression(base, offsets, t)
      case t: Vobla.InterfaceType =>
        val offsets = offset.offsets.map(getIndex(env))
        Vobla.MethodCallExpression(base, Vobla.BuiltInInterfaces.Access(offset.offsets.size), offsets, base.expType.base)
      case t => throw TypeError(t, offset.base.tree)
    }
  }

  private def arrayCst(array: ArrayConstant): Vobla.ArrayConstant = {
    val subs = array.values.map{
      case IndexConstant(i, _) => Vobla.IndexConstant(i)
      case RealConstant(r, _) => Vobla.RealConstant(r)
      case array: ArrayConstant => arrayCst(array)
    }
    val baseType = subs.head.expType
    if(!subs.forall(s => s.expType > baseType)) throw new TypingError("Invalid item types in the array", array.tree)
    val t = baseType match {
      case t: Vobla.ScalarType => Vobla.ArrayType(t, 1)
      case t: Vobla.ArrayType => Vobla.ArrayType(t.base, t.dim + 1)
      case t => throw TypeError(t, array.tree)
    }
    Vobla.ArrayConstant(subs, t)
  }

  // Helpers to deal with names
  private def topLevelName(name: Name) = {
    if(globalNames.contains(name.text)) throw new RedefinedError(name.text, name.tree)
    globalNames += name.text
    localNames += name.text
    name.text
  }

  private def getIdentifier[T1 <: Ast, T2](name: T1, getter: T1 => Option[T2]) =
    getter(name).getOrElse(throw UndefinedError(name.tree.getText, name.tree))

  private val getFunction = (map: Map[String, Vobla.VoblaFunction]) => getIdentifier[Name, Vobla.VoblaFunction](_: Name, s => map.get(s.text))
  private val getStorage = getIdentifier[Name, Vobla.Storage](_: Name, s => storages.get(s.text))
  private val getType = getIdentifier[Name, Vobla.MethodImplementerVersion](_: Name, s => types.get(s.text))
  private val getView = getIdentifier[Name, Vobla.VoblaView](_: Name, s => views.get(s.text))

  private def getInterface(id: InterfaceId) = Vobla.BuiltInInterfaces.interfaces.get(id.name.text) match {
    case None => getIdentifier[InterfaceId, Vobla.Interface](id, id => interfaces.get((id.name.text, id.dim)))
    case Some(i) => i(id.dim)
  }

  private def getVariable(env: Environment)(name: Name) = {
    val v = getIdentifier[Name, Vobla.Expression](name, s => env.varTable.get(s.text))
    if(!v.expType.isInstanceOf[Vobla.ScalarType] && (env.memberVars(v) ^ env.isReturn)) Vobla.ConstExpression(v)
    else v
  }

  private def getInterface(env: Environment)(t: Vobla.Type, tree: Tree) = t match {
    case t: Vobla.InterfaceType => t
    case Vobla.ArrayType(base, dim) => Vobla.InterfaceType(Vobla.BuiltInInterfaces.array(dim), base)
    case t => throw TypeError(t, tree)
  }

  private def getRange(env: Environment)(ast: Ast) = {
    val exp = expression(env)(ast)
    exp.expType match {
      case t: Vobla.RangeType => (exp, t)
      case t =>
        val interface = getInterface(env)(t, ast.tree)
        if(!interface.inter.methodsDeclByName.contains("iterate"))
          throw new TypeError(t, ast.tree)
        val (method, specType) = interface.inter.methodsDeclByName("iterate")
        val methodCall = Vobla.MethodCallExpression(exp, method, Nil, specType.specialize(t.base))
        (methodCall, methodCall.expType.asInstanceOf[Vobla.RangeType])
     }
  }

  // Other helper function and types
  private def getIndex(env: Environment)(ast: Ast) = checkType[Vobla.IndexType](expression(env)(ast), ast.tree)._1
  private def getScalar(env: Environment)(ast: Ast) = checkType[Vobla.ScalarType](expression(env)(ast), ast.tree)._1
  private def getArray(env: Environment)(ast: Ast) = checkType[Vobla.ArrayType](expression(env)(ast), ast.tree)

  private def getVector(env: Environment)(ast: Ast) = {
    val (e, t) = checkType[Vobla.ArrayType](expression(env)(ast), ast.tree)
    if(t.dim != 1) throw TypeError(t, ast.tree)
    (e, t)
  }
  private val getMatrix = checkExpType(_: Environment)({ case Vobla.ArrayType(_, 2) => true; case _ => false }, _: Ast)

  private def checkExpType(env: Environment)(cond: Vobla.Type => Boolean, ast: Ast) =
     check[Vobla.Expression](expression(env)(ast), e => cond(e.expType), a => TypeError(a.expType, ast.tree))

  private def checkType[T: ClassTag](e: Vobla.Expression, t: Tree) = e.expType match {
    case t: T => (e, t)
    case _ => throw TypeError(e.expType, t)
  }

  private def check[T](e: T, cond: T => Boolean, exception: T => TypingError) = if(!cond(e)) throw exception(e) else e

  private def addVar(env: Environment)(name: Name, t: Vobla.Type) = env.varTable.add(Vobla.Variable(name.text, t)).
  getOrElse(throw new RedefinedError(name.text, name.tree))

  private def checkUnicity[T1 <: Ast, T2, T3](names: List[T1], transformer: T1 => (T2, T3)) = names.foldLeft(Map[T2, T3]()){ (s, name) =>
    val item @ (i, _) = transformer(name)
    if(s.contains(i)) throw RedefinedError(name.tree.getText, name.tree)
    s + item
  }.toList

  // Errors
  private class TypingError(val msg: String, val ast: Tree) extends
    UserError(filename + " line " + ast.getLine +  ":" + ast.getCharPositionInLine + " " + msg)
  private case class UndefinedError(name: String, a: Tree) extends TypingError("undeclared name: " + name, a)
  private case class RedefinedError(name: String, a: Tree) extends TypingError("name: " + name + " has already been declared", a)
  private case class UnspecifiedError(name: String, a: Tree) extends TypingError(name + " has not been specified", a)
  private case class TypeError(t: Vobla.Type, a: Tree) extends TypingError("type " + t + " is invalid here", a)

  def typeVoblaProgram(ast: Program): Vobla.Program = {
    ast.delarations.map(topLevelDef)
    new Vobla.Program(functions.toMap.filterKeys(localNames), storages.toMap.filterKeys(localNames), interfaces.toMap.filterKeys(localNames),
        types.toMap.filterKeys(localNames), views.toMap.filterKeys(localNames), exportNames.toList.map{case (name, f) => (name, f())})
  }
}
