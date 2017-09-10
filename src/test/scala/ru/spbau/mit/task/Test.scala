package ru.spbau.mit.task

class Test extends org.scalatest.FunSuite {
  val calculator = new MyCalculator(Array(
    Array(Operator.Binary.ADD, Operator.Binary.SUB),
    Array(Operator.Binary.MUL, Operator.Binary.DIV),
    Array(Operator.Binary.POW),
    Array(Operator.Unary.FAC),
    Array(Function.ABS, Function.COS, Function.SIN)
  ))

  test("simple") {
    assert(calculator.evaluate("-40-1-1") == -42)
    assert(calculator.evaluate("2+2") == 4.0)
    assert(calculator.evaluate("(2+2)*2") == 8.0)
    assert(calculator.evaluate("7/7") == 1.0)
  }

  test("exceptions") {
    assertThrows[ParserException](calculator.evaluate("(2+2"))
    assertThrows[ParserException](calculator.evaluate("1+"))
    assertThrows[ParserException](calculator.evaluate("(0))"))
  }

  test("division by zero") {
    assertThrows[RuntimeException](calculator.evaluate("0/0"))
  }

  test("power") {
    assert(calculator.evaluate("0^0") == 1)
    assert(calculator.evaluate("2^10") == 1024)
  }

  test("factorial") {
    assert(calculator.evaluate("10!") == 3628800)
  }

  test("abs") {
    assert(calculator.evaluate("abs(10)") == 10)
    assert(calculator.evaluate("abs(-10)") == 10)
  }

  test("cos") {
    assert(calculator.evaluate("cos(0)") == 1)
    assert(calculator.evaluate("cos(1)") == Math.cos(1))
  }

  test("sin") {
    assert(calculator.evaluate("sin(0)") == 0)
    assert(calculator.evaluate("sin(1)") == Math.sin(1))
  }
}