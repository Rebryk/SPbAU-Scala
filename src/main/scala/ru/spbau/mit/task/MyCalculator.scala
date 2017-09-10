package ru.spbau.mit.task

class MyCalculator(val operators: Array[Array[Operator]]) extends Calculator {

  // Brackets which are used by the parser.
  object Bracket {
    val OPEN: Char = '('
    val CLOSED: Char = ')'
  }

  /** Evaluates the given expression.
    *
    * @param expression -- expression to evaluate
    * @return the value of the given expression
    */
  override def evaluate(expression: String): Double = evaluateExpression(expression, 0, expression.length)

  /** Evaluates a substring of the given expression.
    *
    * @param expression -- expression to evaluate
    * @param start      -- index of the beginning
    * @param end        -- index of the end
    * @param priority   -- current operator's priority
    * @return the value of substring of the given expression.
    */
  private def evaluateExpression(expression: String, start: Int, end: Int, priority: Int = 0): Double = {
    // empty expression
    if (start == end) {
      throw new ParserException(s"Expression was expected at position $start")
    }

    // expression = '(' + expression + ')', so need to start with priority = 0
    if (expression.charAt(start) == Bracket.OPEN && expression.charAt(end - 1) == Bracket.CLOSED) {
      return evaluateExpression(expression, start + 1, end - 1)
    }

    // if nothing left, try to parse a number
    if (priority == operators.length) {
      try {
        return expression.substring(start, end).toDouble
      } catch {
        case _: Exception => throw new ParserException(s"Number was expected at position $start")
      }
    }

    // find out a type of operators with current priority
    operators(priority)(0) match {
      case _: BinaryOperation => evaluateBinaryOperation(expression, start, end, priority)
      case _ => evaluateUnaryOperation(expression, start, end, priority)
    }
  }

  /** Evaluates a substring of the given expression.
    * Splits expression by binary operation and applies it.
    *
    * @param expression -- expression to evaluate
    * @param start      -- index of the beginning
    * @param end        -- index of the end
    * @param priority   -- current operator's priority
    * @return the value of substring of the given expression.
    */
  private def evaluateBinaryOperation(expression: String, start: Int, end: Int, priority: Int = 0): Double = {
    var bracketCount = 0

    // the result
    var result = 0.0
    var last = start

    // store the last binary operation we have found
    var binary: BinaryOperation = null

    for (startIndex <- start until end) {
      // calculate brackets balance
      expression.charAt(startIndex) match {
        case Bracket.OPEN => bracketCount += 1
        case Bracket.CLOSED => bracketCount -= 1
        case _ => // nothing to do
      }

      if (bracketCount < 0) {
        throw new ParserException(s"Brackets do not match at position $startIndex")
      }

      if (bracketCount == 0) {
        for (operation <- operators(priority)) {
          val endIndex = startIndex + operation.name.length

          // found a binary operation
          if (endIndex <= end && expression.substring(startIndex, endIndex) == operation.name) {
            // the first occurrence
            if (last == start) {
              result = if (startIndex == start) 0 else evaluateExpression(expression, last, startIndex, priority + 1)
            } else {
              result = binary(result, evaluateExpression(expression, last, startIndex, priority + 1))
            }

            binary = operation.asInstanceOf[BinaryOperation]
            last = endIndex
          }
        }
      }
    }

    if (last == end) {
      throw new ParserException(s"Expression was expected at position $last")
    }

    // apply the binary operation to the last operand
    if (binary != null) {
      return binary(result, evaluateExpression(expression, last, end, priority + 1))
    }

    if (bracketCount != 0) {
      throw new ParserException(s"Brackets do not match")
    }

    // try to parse the given expression with next priority
    evaluateExpression(expression, start, end, priority + 1)
  }

  /** Evaluates a substring of the given expression.
    * It tries to find an unary operation at the beginning or end of an expression.
    *
    * @param expression -- expression to evaluate
    * @param start      -- index of the beginning
    * @param end        -- index of the end
    * @param priority   -- current operator's priority
    * @return the value of substring of the given expression.
    */
  private def evaluateUnaryOperation(expression: String, start: Int, end: Int, priority: Int = 0): Double = {
    for (operation <- operators(priority)) {
      operation match {
        // try to match prefix
        case op: PrefixUnaryOperation =>
          val endIndex = start + operation.name.length

          if (endIndex <= end && expression.substring(start, endIndex) == operation.name) {
            return op(evaluateExpression(expression, endIndex, end, priority))
          }

        // try to match suffix
        case op: SuffixUnaryOperation =>
          val startIndex = end - operation.name.length

          if (startIndex >= start && expression.substring(startIndex, end) == operation.name) {
            return op(evaluateExpression(expression, start, startIndex, priority))
          }

        case _ => throw new FatalException(s"Unary operation was expected")
      }
    }

    // try to parse the given expression with next priority
    evaluateExpression(expression, start, end, priority + 1)
  }
}
