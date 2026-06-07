package calc

import calc.error.*
import calc.parser.IdentifierTable
import munit.Location

class EvaluationSuite extends munit.FunSuite {
  override def beforeEach(context: BeforeEach): Unit =
    IdentifierTable.UserVariables = Map()

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

  // ── Builtin functions ──
  test("sin function") {
    customAssertEqual(eval("sin(0)"), Right(0.0))
    customAssertEqual(eval("sin(pi / 2)"), Right(1.0))
    customAssertEqual(eval("sin(pi)"), Right(0.0))
    customAssertEqual(eval("sin(3 * pi / 2)"), Right(-1.0))
  }

  test("cos function") {
    customAssertEqual(eval("cos(0)"), Right(1.0))
    customAssertEqual(eval("cos(pi)"), Right(-1.0))
    customAssertEqual(eval("cos(pi / 2)"), Right(0.0))
  }

  test("builtin functions in expressions") {
    customAssertEqual(eval("sin(0) + cos(0)"), Right(1.0))
    customAssertEqual(eval("2 * sin(pi / 2)"), Right(2.0))
    customAssertEqual(eval("sin(pi / 4) ^ 2 + cos(pi / 4) ^ 2"), Right(1.0))
  }

  test("builtin functions with implicit multiplication") {
    customAssertEqual(eval("2 sin(pi / 2)"), Right(2.0))
    customAssertEqual(eval("pi sin(pi / 2)"), Right(math.Pi))
  }

  test("builtin functions missing parentheses") {
    customAssertEqual(eval("sin"), Left(ParsingInvalidUsesofFunctions("sin")))
    customAssertEqual(eval("cos pi"), Left(ParsingInvalidUsesofFunctions("cos")))
  }

  test("builtin functions as variables") {
    customAssertEqual(eval("x = sin"), Left(ParsingInvalidUsesofFunctions("sin")))
  }

  test("builtin functions with nested parentheses") {
    customAssertEqual(eval("sin((pi / 2))"), Right(1.0))
    customAssertEqual(eval("cos((pi))"), Right(-1.0))
  }

  // ── Trigonometric: tan, csc, sec, cot ──
  test("tan function") {
    customAssertEqual(eval("tan(0)"), Right(0.0))
    customAssertEqual(eval("tan(pi / 4)"), Right(1.0))
  }

  test("csc function") {
    customAssertEqual(eval("csc(pi / 2)"), Right(1.0))
    customAssertEqual(eval("csc(pi / 6)"), Right(2.0))
  }

  test("sec function") {
    customAssertEqual(eval("sec(0)"), Right(1.0))
    customAssertEqual(eval("sec(pi / 3)"), Right(2.0))
  }

  test("cot function") {
    customAssertEqual(eval("cot(pi / 4)"), Right(1.0))
    customAssertEqual(eval("cot(pi / 2)"), Right(0.0))
  }

  // ── Hyperbolic functions ──
  test("sinh function") {
    customAssertEqual(eval("sinh(0)"), Right(0.0))
  }

  test("cosh function") {
    customAssertEqual(eval("cosh(0)"), Right(1.0))
  }

  test("tanh function") {
    customAssertEqual(eval("tanh(0)"), Right(0.0))
  }

  // ── Inverse trigonometric functions ──
  test("asin function") {
    customAssertEqual(eval("asin(0)"), Right(0.0))
    customAssertEqual(eval("asin(1)"), Right(math.Pi / 2))
  }

  test("acos function") {
    customAssertEqual(eval("acos(1)"), Right(0.0))
    customAssertEqual(eval("acos(0)"), Right(math.Pi / 2))
  }

  test("atan function") {
    customAssertEqual(eval("atan(0)"), Right(0.0))
    customAssertEqual(eval("atan(1)"), Right(math.Pi / 4))
  }

  test("acsc function") {
    customAssertEqual(eval("acsc(1)"), Right(math.Pi / 2))
  }

  test("asec function") {
    customAssertEqual(eval("asec(1)"), Right(0.0))
  }

  test("acot function") {
    customAssertEqual(eval("acot(1)"), Right(math.Pi / 4))
  }

  // ── Logarithmic functions ──
  test("ln function") {
    customAssertEqual(eval("ln(1)"), Right(0.0))
    customAssertEqual(eval("ln(e)"), Right(1.0))
  }

  test("log2 function") {
    customAssertEqual(eval("log2(1)"), Right(0.0))
    customAssertEqual(eval("log2(2)"), Right(1.0))
    customAssertEqual(eval("log2(8)"), Right(3.0))
  }

  test("log10 function") {
    customAssertEqual(eval("log10(1)"), Right(0.0))
    customAssertEqual(eval("log10(10)"), Right(1.0))
    customAssertEqual(eval("log10(100)"), Right(2.0))
  }

  // ── sqrt, ceil, floor, abs ──
  test("sqrt function") {
    customAssertEqual(eval("sqrt(0)"), Right(0.0))
    customAssertEqual(eval("sqrt(4)"), Right(2.0))
    customAssertEqual(eval("sqrt(9)"), Right(3.0))
  }

  test("ceil function") {
    customAssertEqual(eval("ceil(2.3)"), Right(3.0))
    customAssertEqual(eval("ceil(2.0)"), Right(2.0))
    customAssertEqual(eval("ceil(-2.3)"), Right(-2.0))
  }

  test("floor function") {
    customAssertEqual(eval("floor(2.7)"), Right(2.0))
    customAssertEqual(eval("floor(2.0)"), Right(2.0))
    customAssertEqual(eval("floor(-2.7)"), Right(-3.0))
  }

  test("abs function") {
    customAssertEqual(eval("abs(5)"), Right(5.0))
    customAssertEqual(eval("abs(-5)"), Right(5.0))
    customAssertEqual(eval("abs(0)"), Right(0.0))
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
    customAssertEqual(eval("sin"), Left(ParsingInvalidUsesofFunctions("sin")))
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
    customAssertEqual(eval("sin = 5"), Left(ParsingInvalidUsesofFunctions("sin")))
    customAssertEqual(eval("cos = 3"), Left(ParsingInvalidUsesofFunctions("cos")))
  }
}
