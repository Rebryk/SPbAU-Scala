package ru.spbau.mit.task

trait Operator {
  val name: String
}

trait Calculator {
  def evaluate(expression: String): Double
}

abstract class BinaryOperation(val name: String) extends Operator {
  def apply(operandLeft: Double, operandRight: Double): Double
}

abstract class PrefixUnaryOperation(val name: String) extends Operator {
  def apply(operand: Double): Double
}

abstract class SuffixUnaryOperation(val name: String) extends Operator {
  def apply(operand: Double): Double
}

/**
  * Class implements binary operation.
  *
  * Examples of usage:
  * 'expression' 'operation' 'expression'
  * a + b
  * a * b
  *
  * @param name -- the name of binary operation. The parser splits an expression by this name.
  * @param func -- the function to run when this operation is called.
  */
class DefaultBinaryOperation(name: String, val func: (Double, Double) => Double) extends BinaryOperation(name) {
  override def apply(operandLeft: Double, operandRight: Double): Double = func(operandLeft, operandRight)
}

/**
  * Class implements prefix unary operation.
  *
  * Examples of usage:
  * 'operation'('expression')
  * 'operation' 'expression'
  * cos(0)
  * abs(-10)
  *
  * @param name -- the name of prefix unary operation. The parser split an expression by this name.
  * @param func -- the function to run when this operation is called.
  */
class DefaultPrefixUnaryOperation(name: String, val func: (Double) => Double) extends PrefixUnaryOperation(name) {
  override def apply(operand: Double): Double = func(operand)
}

/**
  * Class implements suffix unary operation.
  *
  * Examples of usage:
  * ('expression')'operation'
  * 'expression' 'operation'
  * 10!
  *
  * @param name -- the name of suffix unary operation. The parser split an expression by this name.
  * @param func -- the function to run when this operation is called.
  */
class DefaultSuffixUnaryOperation(name: String, val func: (Double) => Double) extends SuffixUnaryOperation(name) {
  override def apply(operand: Double): Double = func(operand)
}


/** A collection of implemented operators.
  *
  * Binary operations: +, -, *, /
  * Unary operations: !
  */
object Operator {

  object Binary {
    val ADD = new DefaultBinaryOperation("+", (a, b) => a + b)

    val SUB = new DefaultBinaryOperation("-", (a, b) => a - b)

    val MUL = new DefaultBinaryOperation("*", (a, b) => a * b)

    val DIV = new DefaultBinaryOperation("/", (a, b) => {
      b match {
        case 0 => throw new RuntimeException("Division by zero")
        case _ => a / b
      }
    })

    val POW = new DefaultBinaryOperation("^", (a, b) => Math.pow(a, b))
  }

  object Unary {
    val FAC = new DefaultSuffixUnaryOperation("!", (a) => {
      val value = a.toInt

      if (value < 0) {
        throw new RuntimeException(s"$a is invalid argument for factorial")
      }

      var result = 1
      for (i <- 2 to value) {
        result *= i
      }

      result
    })
  }

}

/** A collection of implemented functions.
  * Actually, they are unary operators too.
  *
  * Functions: abs, cos, sin
  */
object Function {
  val ABS = new DefaultPrefixUnaryOperation("abs", (a) => Math.abs(a))

  val COS = new DefaultPrefixUnaryOperation("cos", (a) => Math.cos(a))

  val SIN = new DefaultPrefixUnaryOperation("sin", (a) => Math.sin(a))
}



