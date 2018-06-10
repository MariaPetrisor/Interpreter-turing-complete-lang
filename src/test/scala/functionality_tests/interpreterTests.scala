package functionality_tests

import org.scalatest.FunSpec
import interpreter._

class interpreterTests extends FunSpec {
  describe("Addition test"){
    it("simple addition") {
      //<add> <const> 1 <const> 1
      val expected = Const(2)
      val env = Env
      val addition = Add(Const(1), Const(1))
      val result = env.evaluate(addition, Map(), List())
      assert(expected == result)
    }

    it("complex addition"){
      //<add>
      //  <add> <const> 1 <const> 1
      //  <add> <const> 2 <const> 2
      val expected = Const(6)
      val env = Env
      val addition1 = Add(Const(1), Const(1))
      val addition2 = Add(Const(2), Const(2))
      val result = env.evaluate(Add(addition1, addition2), Map(), List())
      assert(expected == result)
    }

    it("even more complex addition"){
      //<add>
      //  <add>
      //    <add> <const> 1 <const> 1
      //    <add> <const> 2 <const> 2
      //  <add>
      //    <add> <const> 1 <const> 1
      //    <add> <const> 2 <const> 2
      val expected = Const(12)
      val env = Env
      val addition1 = Add(Const(1), Const(1))
      val addition2 = Add(Const(2), Const(2))
      val l = Add(addition1, addition2)
      val r = Add(addition2, addition1)
      val result = env.evaluate(Add(l, r), Map(), List())
      assert(expected == result)
    }
  }

  describe("Subtraction test"){
    it("simple subtraction") {
      //<sub> <const> 2 <const> 1
      val expected = Const(1)
      val env = Env
      val sub = Sub(Const(2), Const(1))
      val result = env.evaluate(sub, Map(), List())
      assert(expected == result)
    }
  }

  describe("Multiplication test"){
    it("simple multiplication") {
      //<mul> <const> 2 <const> 2
      val expected = Const(4)
      val env = Env
      val mul = Mul(Const(2), Const(2))
      val result = env.evaluate(mul, Map(), List())
      assert(expected == result)
    }
  }

  describe("Division test") {
    it("simple division") {
      //<diff> <const> 4 <const> 2
      val expected = Const(2)
      val env = Env
      val diff = Diff(Const(4), Const(2))
      val result = env.evaluate(diff, Map(), List())
      assert(expected == result)
    }
  }

  describe("Equality test") {
    it("Int equality true") {
      //<eq> <const> 2 <const> 2
      val expected = Const(1)
      val env = Env
      val eq = Eq(Const(2), Const(2))
      val result = env.evaluate(eq, Map(), List())
      assert(expected == result)
    }


    it("Int equality false") {
      //<eq> <const> 2 <const> 3
      val expected = Const(0)
      val env = Env
      val eq = Eq(Const(2), Const(3))
      val result = env.evaluate(eq, Map(), List())
      assert(expected == result)
    }
  }

  describe("If test"){
    it("if then"){
      //<if> <eq> <const> 2 <add> <const> 1 <const> 1
      //  <add> <const> 1 <const> 1
      //  <mul> <const> 2 <const> 2
      val expected = Const(2)
      val env = Env
      val if_cond = Eq(Const(2), Add(Const(1), Const(1))) //true
      val branch_then = Add(Const(1), Const(1)) //2
      val branch_else = Mul(Const(2), Const(2)) //4
      val if_stat = If(if_cond, branch_then, branch_else)
      val result = env.evaluate(if_stat, Map(), List())
      assert(expected == result)
    }

    it("if else") {
      //<if> <eq> <const> 2 <add> <const> 2 <const> 1
      //  <add> <const> 1 <const> 1
      //  <mul> <const> 2 <const> 2
      val expected = Const(4)
      val env = Env
      val if_cond = Eq(Const(2), Add(Const(2), Const(1))) //false
      val branch_then = Add(Const(1), Const(1)) //2
      val branch_else = Mul(Const(2), Const(2)) //4
      val if_stat = If(if_cond, branch_then, branch_else)
      val result = env.evaluate(if_stat, Map(), List())
      assert(expected == result)
    }
  }

  describe("Val decl test"){
    it("simple val decl"){
      //<val-decl> "x" <const> 42 <mul> <val> "x" <const> 42
      val expected = Const(1764)
      val env = Env
      val val_decl = ValDecl(Map(Val("x") -> Const(42)), Mul(Val("x"), Const(42)))
      val result = env.evaluate(val_decl, Map(), List())
      assert(expected == result)
    }

    it("shadow val decl") {
      val expected = Const(4)
      val env = Env
      val function = Add(Val("x"), Val("y"))
      //exprShadow = Let (Var "x") (Const 1) (Var "y") (Const 2)
      //                Let (Var "x") (Const 3)
      //                  in x + y
      val shadow_val = ValDecl(Map(Val("y") -> Const(3)), function)
      val exprShadow = ValDecl(Map(Val("x") -> Const(1), Val("y") -> Const(2)), shadow_val)
      val result = env.evaluate(exprShadow, Map(), List())

      assert(expected == result)
    }
  }

  describe("Lambda apply test"){
    it("simple lambda apply"){
      val expected = Const(42)
      val env = Env
      //        (Apply
      //          (Lambda [Var "x", Var "y"]
      //            ((Var "x") :*: (Var "y"))
      //          )
      //          [Const (IntVal 6), Const (IntVal 7)]
      //        )

      val function = Mul(Val("x"), Val("y"))
      val lam = Lambda(List(Val("x"), Val("y")), function)
      val app = Apply(lam, List(Const(6), Const(7)))
      val result = env.evaluate(app, Map(), List())
      assert(expected == result)
    }

    it("currying"){
      val expected = Const(42)
      val env = Env
      //        (Apply
      //          (Apply
      //            (Lambda [Var "x"]
      //              (Lambda [Var "y"]
      //                ((Var "x") :*: (Var "y"))
      //              )
      //            )
      //          ([Const (IntVal 6)])
      //          )
      //        ([Const (IntVal 7)])
      //        )

      val function = Mul(Val("x"), Val("y"))
      val lam = Lambda(List(Val("x")), Lambda(List(Val("y")), function))
      val app = Apply(Apply(lam, List(Const(6))), List(Const(7)))
      val result = env.evaluate(app, Map(), List())

      assert(expected == result)
    }
  }

  describe("empty val-decl list") {
    it("check exception") {
      val env = Env
      val thrown = intercept[emptyListException] {
        env.evaluate(ValDecl(Map(), Val("x")), Map(), List())
      }
      assert(thrown.getMessage === "Variable list in val-decl should not be empty")
    }
  }
}
