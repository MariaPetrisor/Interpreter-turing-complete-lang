package interpreter

sealed trait Expr

case class Lambda(args: List[Val], body: Expr) extends Expr
case class Val(name: String) extends Expr
case class Apply(fun: Expr, args: List[Expr]) extends Expr
case class Const(int: Integer) extends Expr

trait BinaryOperation extends Expr{
  def expr1: Expr
  def expr2: Expr
}

case class Add(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Sub(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Mul(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Diff(expr1: Expr, expr2: Expr) extends BinaryOperation

case class Eq(expr1: Expr, expr2: Expr) extends Expr
case class If(cond: Expr, expr1: Expr, expr2: Expr) extends Expr
case class ValDecl(var_value: Map[Val, Expr], body: Expr) extends Expr

object Env extends App {
  def evaluate(expr: Expr, env: Map[Val, Expr], queue: List[Expr]): Expr = expr match {
    case Val(name) =>
      if(env contains Val(name)){
        env.apply(Val(name))
      }
      else {
        Val(name)
      }
    case Add(lhs, rhs) => (lhs, rhs) match {
      case (Const(int1), Const(int2)) => Const(int1 + int2)
      case (Const(int1), _) => evaluate(Add(Const(int1), evaluate(rhs, env, queue)), env, queue)
      case (_, Const(int2)) => evaluate(Add(evaluate(lhs, env, queue), Const(int2)), env, queue)
      case (_, _) => evaluate(Add(evaluate(lhs, env, queue), evaluate(rhs, env, queue)), env, queue)
    }

    case Sub(lhs, rhs) => (lhs, rhs) match {
      case (Const(int1), Const(int2)) => Const(int1 - int2)
      case (Const(int1), _) => evaluate(Sub(Const(int1), evaluate(rhs, env, queue)), env, queue)
      case (_, Const(int2)) => evaluate(Sub(evaluate(lhs, env, queue), Const(int2)), env, queue)
      case (_, _) => evaluate(Sub(evaluate(lhs, env, queue), evaluate(rhs, env, queue)), env, queue)
    }

    case Mul(lhs, rhs) => (lhs, rhs) match {
      case (Const(int1), Const(int2)) => Const(int1 * int2)
      case (Const(int1), _) => evaluate(Mul(Const(int1), evaluate(rhs, env, queue)), env, queue)
      case (_, Const(int2)) => evaluate(Mul(evaluate(lhs, env, queue), Const(int2)), env, queue)
      case (_, _) => evaluate(Mul(evaluate(lhs, env, queue), evaluate(rhs, env, queue)), env, queue)
    }

    case Diff(lhs, rhs) => (lhs, rhs) match {
      case (Const(int1), Const(int2)) => Const(int1 / int2)
      case (Const(int1), _) => evaluate(Diff(Const(int1), evaluate(rhs, env, queue)), env, queue)
      case (_, Const(int2)) => evaluate(Diff(evaluate(lhs, env, queue), Const(int2)), env, queue)
      case (_, _) => evaluate(Diff(evaluate(lhs, env, queue), evaluate(rhs, env, queue)), env, queue)
    }

    case Eq(lhs, rhs) => (lhs, rhs) match {
      case (Const(int1), Const(int2)) => Const(if(int1 == int2) 1 else 0)
      case (Const(int1), _) => evaluate(Diff(Const(int1), evaluate(rhs, env, queue)), env, queue)
      case (_, Const(int2)) => evaluate(Diff(evaluate(lhs, env, queue), Const(int2)), env, queue)
      case (_, _) => evaluate(Diff(evaluate(lhs, env, queue), evaluate(rhs, env, queue)), env, queue)
    }

    case If(cond, branch1, branch2) => cond match {
      case Const(int) => if(int == 1) evaluate(branch1, env, queue) else evaluate(branch2, env, queue)
      case _ => evaluate(If(evaluate(cond, env, queue), branch1, branch2), env, queue)
    }

    case ValDecl(var_value, body) =>
      if(var_value.isEmpty) {
        throw emptyListException("Variable list in val-decl should not be empty")
      }
      else {
        evaluate(body, env ++ var_value, queue)
      }

    case Apply(fun, expr_list) => evaluate(fun, env, queue ++ expr_list)
    case Lambda(string_list, body) =>
      val (new_env, new_queue) = updateEnv(env, queue, string_list)
      evaluate(body, new_env, new_queue)
  }


  def updateEnv(env: Map[Val, Expr], queue: List[Expr], stringList: List[Val]): (Map[Val, Expr], List[Expr]) = stringList match {
    case Nil => (env, queue)
    case head :: tail if queue.nonEmpty => updateEnv(env + (head -> queue.head), queue.tail, tail)
    case _ => (env, queue)
  }

  def exprToString(expr: Expr): String = expr match {
    case Const(int) => int.toString
    case Val(name) => name
    case Lambda(args, body) => String.format("λ%s -> %s", args.map(x => exprToString(x)).mkString(", "), exprToString(body))
    case Apply(fun, args) => String.format("(%s) (%s)", exprToString(fun), args.map(x => exprToString(x)).mkString(", "))
    case Add(lhs, rhs) => String.format("%s + %s", exprToString(lhs), exprToString(rhs))
  }

  def freeVariables(expr: Expr): Set[Val] = expr match {
    case Val(name) => Set(Val(name))
    case Lambda(args, body) => args match{
      case Nil => freeVariables(body)
      case head :: tail => freeVariables(Lambda(tail, body)) -- freeVariables(head)
    }
    case Apply(fun, args) => args match {
      case Nil => freeVariables(fun)
      case head :: tail => freeVariables(Apply(fun, tail)) union freeVariables(head)
    }
    case Add(lhs, rhs) => freeVariables(lhs) union freeVariables(rhs)
  }

  def rename(vall: Val, vals: Set[String]): Val = {
    if (vals contains exprToString(vall)) {
      rename(Val(exprToString(vall) + "'"), vals)
    }
    else {
      vall
    }
  }
}


//  def substitute(original: Val, term: Expr, expr: Expr): Expr = expr match {
//    case Val(name) => if(exprToString(original).equals(name)){
//      term
//    }
//      else{
//      original
//    }
//    case Lambda(args, body) =>
//      if(exprToString(original).equals(exprToString(arg))){
//        Lambda(arg, body)
//      }
//      else if(!(freeVariables(body) contains exprToString(original))){
//        Lambda(arg, body)
//      }
//      else if(freeVariables(term) contains exprToString(arg)){
//        Lambda(rename(arg, freeVariables(body) union freeVariables(term)), substitute(original, term, body))
//      }
//      else {
//        Lambda(arg, substitute(original, term, body))
//      }
//    case Apply(fun, arg) => Apply(substitute(original, term, fun), substitute(original, term, arg))
//  }

//  def isBetaReductionPossible(expr: Expr): Boolean = expr match {
//    case Apply(Lambda(arg, body), s) => true
//    case _ => false
//  }

//  def betaReduction(expr: Expr): Expr = expr match {
//    case Apply(Lambda(arg, body), s) => substitute(arg, s, body)
//    case _ => expr
//  }
