package calc

import calc.error.*
import calc.parser.IdentifierTable
import munit.Location

class EvaluationSuite extends munit.FunSuite {
  override def beforeEach(context: BeforeEach): Unit =
    IdentifierTable.UserDefinedVariables = Map()

  def customAssertEqual(evalResult: Either[CustomError, Double], expectedResult: Either[CustomError, Double])(implicit
      loc: Location
  ): Unit =
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
    customAssertEqual(eval("2 ^ 3 ^ 2"), Right(math.pow(2, math.pow(3, 2)))) // right-associative
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

  // ── Builtin constants cases ──
  test("builtin constants") {
    customAssertEqual(eval("e"), Right(math.E))
    customAssertEqual(eval("pi"), Right(math.Pi))
  }

  // ── Unary operators ──
  test("unary minus") {
    customAssertEqual(eval("-5"), Right(-5.0))
    customAssertEqual(eval("-5 + 3"), Right(-2.0))
    customAssertEqual(eval("3 + -5"), Right(-2.0))
    customAssertEqual(eval("-(2 + 3)"), Right(-5.0))
    customAssertEqual(eval("--5"), Right(5.0))
    customAssertEqual(eval("-e"), Right(-1 * Math.E))
    customAssertEqual(eval("--pi"), Right(-1 * -1 * Math.PI))
  }

  test("unary plus") {
    customAssertEqual(eval("+5"), Right(5.0))
    customAssertEqual(eval("+5 - 3"), Right(2.0))

    customAssertEqual(eval("+pi - e"), Right(Math.PI - Math.E))
  }

  // ── Implicit multiplication ──
  test("implicit multiplication") {
    customAssertEqual(eval("2(3)"), Right(6.0))
    customAssertEqual(eval("(2)(3)"), Right(6.0))
    customAssertEqual(eval("(2 + 1)(3)"), Right(9.0))

    customAssertEqual(eval("2e"), Right(2 * Math.E))
    customAssertEqual(eval("2 pi e"), Right(2 * Math.PI * Math.E))
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

  test("invalid mathematical expression") {
    customAssertEqual(eval("(1 +"), Left(ParsingInvalidMathExpression))
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

  test("invalid variable name") {
    customAssertEqual(eval("sin"), Left(NameResolutionVariableNameShadowsFunctionName("sin")))
  }

  test("variable doesn't exists") {
    customAssertEqual(
      eval("ImJustAnUndefinedVariable"),
      Left(NameResolutionVariableDoesntExists("ImJustAnUndefinedVariable"))
    )
  }

  // ── Assignment ──
  test("basic assignment") {
    customAssertEqual(eval("x = 5"), Right(5.0))
    customAssertEqual(eval("x"), Right(5.0))
  }

  test("assignment with expression") {
    customAssertEqual(eval("x = 2 + 3"), Right(5.0))
    customAssertEqual(eval("x"), Right(5.0))
  }

  test("assignment with builtin constants") {
    customAssertEqual(eval("x = pi"), Right(math.Pi))
    customAssertEqual(eval("x"), Right(math.Pi))

    customAssertEqual(eval("y = e"), Right(math.E))
    customAssertEqual(eval("y"), Right(math.E))
  }

  test("overwrite existing variable") {
    customAssertEqual(eval("x = 10"), Right(10.0))
    customAssertEqual(eval("x = 20"), Right(20.0))
    customAssertEqual(eval("x"), Right(20.0))
  }

  test("assignment with complex expression") {
    customAssertEqual(eval("x = (1 + 2) * 3 - 4 / 2"), Right((1 + 2) * 3 - 4.0 / 2.0))
    customAssertEqual(eval("x"), Right((1 + 2) * 3 - 4.0 / 2.0))
  }

  test("use variable in expression") {
    customAssertEqual(eval("x = 5"), Right(5.0))
    customAssertEqual(eval("x + 3"), Right(8.0))
    customAssertEqual(eval("x * 2"), Right(10.0))
    customAssertEqual(eval("x ^ 2"), Right(25.0))
  }

  test("implicit multiplication with variables") {
    customAssertEqual(eval("x = 3"), Right(3.0))
    customAssertEqual(eval("2x"), Right(6.0))
    customAssertEqual(eval("x x"), Right(9.0))
  }

  test("assignment with unary minus") {
    customAssertEqual(eval("x = -5"), Right(-5.0))
    customAssertEqual(eval("x"), Right(-5.0))

    customAssertEqual(eval("y = -(2 + 3)"), Right(-5.0))
    customAssertEqual(eval("y"), Right(-5.0))
  }

  test("error: assignment to non-variable") {
    customAssertEqual(eval("5 = 3"), Left(ParsingInvalidMathExpression))
    customAssertEqual(eval("(x) = 5"), Left(ParsingInvalidMathExpression))
    customAssertEqual(eval("(1 + 2) = 5"), Left(ParsingInvalidMathExpression))
  }

  test("error: assign to reserved function name") {
    customAssertEqual(eval("sin = 5"), Left(NameResolutionVariableNameShadowsFunctionName("sin")))
    customAssertEqual(eval("cos = 3"), Left(NameResolutionVariableNameShadowsFunctionName("cos")))
  }
}
