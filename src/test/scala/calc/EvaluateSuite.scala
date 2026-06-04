package calc

import calc.error.*
import munit.Location

class EvaluationSuite extends munit.FunSuite {
  def customAssertEqual(evalResult: Either[CustomError, Double], expectedResult: Either[CustomError, Double])(implicit loc: Location): Unit =
    (evalResult, expectedResult) match
      case (Left(error1), Left(error2)) =>
        assertEquals(error1, error2)
      case (Right(value1), Right(value2)) =>
        assertEqualsDouble(value1, value2, 0.000001)
      case (Left(err), _) =>
        fail(s"Expected $expectedResult but got error: ${err.message}")
      case (_, Left(err)) =>
        fail(s"Got $evalResult but expected error: ${err.message}")


  // ── Basic arithmetic ──
  test("addition") {
    customAssertEqual(eval("1 + 2 + 3"), Right(1 + 2 + 3))
    customAssertEqual(eval("0.5 + 0.25"), Right(0.75))
    customAssertEqual(eval(".5 + .25"), Right(0.75))
  }

  test("subtraction") {
    customAssertEqual(eval("1 - 2 - 3"), Right(1 - 2 - 3))
    customAssertEqual(eval("10 - 5"), Right(5.0))
  }

  test("multiplication") {
    customAssertEqual(eval("4 * 2 * 3"), Right(4 * 2 * 3))
    customAssertEqual(eval("2.5 * 4"), Right(10.0))
  }

  test("division") {
    customAssertEqual(eval("4 / 2 / 3"), Right(4.0 / 2.0 / 3.0))
    customAssertEqual(eval("10 / 4"), Right(2.5))
  }

  test("exponentiation") {
    customAssertEqual(eval("2 ^ 3"), Right(8.0))
    customAssertEqual(eval("2 ^ 3 ^ 2"), Right(math.pow(2, math.pow(3, 2))))  // right-associative
  }

  // ── Operator precedence ──
  test("precedence: * before +") {
    customAssertEqual(eval("1 + 2 * 3"), Right(7.0))
    customAssertEqual(eval("2 * 3 + 1"), Right(7.0))
  }

  test("precedence: / before +") {
    customAssertEqual(eval("1 + 2 / 3"), Right(1 + (2.0 / 3)))
    customAssertEqual(eval("1 + 4 / 2"), Right(3.0))
  }

  test("precedence: ^ before -") {
    customAssertEqual(eval("1 - 2 ^ 3"), Right(1 - math.pow(2, 3)))
    customAssertEqual(eval("2 ^ 3 - 4"), Right(4.0))
  }

  // ── Parentheses ──
  test("parentheses override precedence") {
    customAssertEqual(eval("(1 + 2) * 3"), Right(9.0))
    customAssertEqual(eval("3 * (1 + 2)"), Right(9.0))
    customAssertEqual(eval("(2 + 3) ^ 2"), Right(25.0))
  }

  test("nested parentheses") {
    customAssertEqual(eval("((1 + 2) * 3)"), Right(9.0))
    customAssertEqual(eval("(3 * (1 + 2))"), Right(9.0))
    customAssertEqual(eval("((2 + 3) * (4 + 1))"), Right(25.0))
  }

  // ── Unary operators ──
  test("unary minus") {
    customAssertEqual(eval("-5"), Right(-5.0))
    customAssertEqual(eval("-5 + 3"), Right(-2.0))
    customAssertEqual(eval("3 + -5"), Right(-2.0))
    customAssertEqual(eval("-(2 + 3)"), Right(-5.0))
    customAssertEqual(eval("--5"), Right(5.0))
  }

  test("unary plus") {
    customAssertEqual(eval("+5"), Right(5.0))
    customAssertEqual(eval("+5 - 3"), Right(2.0))
  }

  // ── Implicit multiplication ──
  test("implicit multiplication via parentheses") {
    customAssertEqual(eval("2(3)"), Right(6.0))
    customAssertEqual(eval("(2)(3)"), Right(6.0))
    customAssertEqual(eval("(2 + 1)(3)"), Right(9.0))
  }

  // ── Mixed expressions ──
  test("complex expressions") {
    customAssertEqual(eval("(1 + 2) * 3 - 4 / 2"), Right((1 + 2) * 3 - 4.0 / 2.0))
    customAssertEqual(eval("2 ^ 3 + 4 * 5"), Right(math.pow(2, 3) + 4 * 5))
    customAssertEqual(eval("(1.5 + 2.5) * (10 - 6)"), Right(16.0))
  }

  // ── Whitespace ──
  test("whitespace handling") {
    customAssertEqual(eval("  1  +  2  "), Right(3.0))
    customAssertEqual(eval("\t1\t+\t2"), Right(3.0))
  }

  // ── Error cases ──
  test("empty expression") {
    customAssertEqual(eval(""), Left(ParsingEmptyExpression))
  }

//  test("invalid characters") {
//    customAssertEqual(eval("1 + a"), Left(TokenizationInvalidCharacterError))
//    customAssertEqual(eval("1 + @"), Left(TokenizationInvalidCharacterError))
//  }

  test("invalid bracket sequence") {
    customAssertEqual(eval("(1 + 2"), Left(ParsingInvalidBracketSequence))
    customAssertEqual(eval("1 + 2)"), Left(ParsingInvalidBracketSequence))
  }

  test("missing operator") {
    customAssertEqual(eval("1 2"), Left(ParsingMissingOperator))
  }

  test("invalid math expression (leading */^)") {
    customAssertEqual(eval("*5"), Left(ParsingInvalidMathExpression))
    customAssertEqual(eval("/5"), Left(ParsingInvalidMathExpression))
    customAssertEqual(eval("^5"), Left(ParsingInvalidMathExpression))
  }

  test("invalid numerical value") {
    customAssertEqual(eval("1.2.3"), Left(TokenizationInvalidNumericalValueError))
  }
}
