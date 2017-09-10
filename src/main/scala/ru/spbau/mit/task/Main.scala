package ru.spbau.mit.task

import scala.io.StdIn

object Main {
  def run(calculator: Calculator): Unit = {
    var isRunning = true

    while (isRunning) {
      val input = StdIn.readLine().filter(it => it != ' ')

      // do until "exit"
      if (input == "exit") {
        isRunning = false
      } else {
        try {
          println(s"Result: ${calculator.evaluate(input)}")
        } catch {
          case e: Exception => println(s"Error: ${e.getMessage}")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // build a calculator
    // setup all allowed operators
    val calculator = new MyCalculator(Array(
      Array(Operator.Binary.ADD, Operator.Binary.SUB),
      Array(Operator.Binary.MUL, Operator.Binary.DIV),
      Array(Operator.Binary.POW),
      Array(Operator.Unary.FAC),
      Array(Function.ABS, Function.COS, Function.SIN)
    ))

    run(calculator)
  }
}
