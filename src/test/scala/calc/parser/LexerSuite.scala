package calc.parser

import calc.error.*

class LexerSuite extends munit.FunSuite {
  test("Integer and Double Literals") {
    assertEquals(
      lex("67 1.67 .67"),
      Right(List(DoubleLiteral(67), DoubleLiteral(1.67), DoubleLiteral(0.67)))
    )
    assertEquals(
      lex("-67   -1.67     -.67"),
      Right(
        List(
          Opt('-'),
          DoubleLiteral(67),
          Opt('-'),
          DoubleLiteral(1.67),
          Opt('-'),
          DoubleLiteral(0.67)
        )
      )
    )
    assertEquals(
      lex("---5 "),
      Right(List(Opt('-'), Opt('-'), Opt('-'), DoubleLiteral(5)))
    )

    assertEquals(lex("65.66.67"), Left(TokenizationInvalidNumericalValueError))
    assertEquals(lex(".66.67"), Left(TokenizationInvalidNumericalValueError))
  }

  test("Expressions with only literals") {
    assertEquals(
      lex("1 + 2 - 3 * 4 / 5 ^ 6"),
      Right(
        List(
          DoubleLiteral(1),
          Opt('+'),
          DoubleLiteral(2),
          Opt('-'),
          DoubleLiteral(3),
          Opt('*'),
          DoubleLiteral(4),
          Opt('/'),
          DoubleLiteral(5),
          Opt('^'),
          DoubleLiteral(6)
        )
      )
    )
    assertEquals(
      lex("1.5 + 2.5 - 3.5 * 4.5 / 5.5 ^ 6.5"),
      Right(
        List(
          DoubleLiteral(1.5),
          Opt('+'),
          DoubleLiteral(2.5),
          Opt('-'),
          DoubleLiteral(3.5),
          Opt('*'),
          DoubleLiteral(4.5),
          Opt('/'),
          DoubleLiteral(5.5),
          Opt('^'),
          DoubleLiteral(6.5)
        )
      )
    )
  }

  test("Identifiers") {
    assertEquals(lex("x"), Right(List(Ident("x"))))
    assertEquals(lex("result"), Right(List(Ident("result"))))
    assertEquals(lex("x1 y2"), Right(List(Ident("x1"), Ident("y2"))))
    assertEquals(lex("abc def"), Right(List(Ident("abc"), Ident("def"))))
  }

  test("Parentheses") {
    assertEquals(lex("()"), Right(List(LeftParen, RightParen)))
    assertEquals(lex("(  )"), Right(List(LeftParen, RightParen)))
    assertEquals(lex("(1)"), Right(List(LeftParen, DoubleLiteral(1), RightParen)))
    assertEquals(
      lex("((x))"),
      Right(List(LeftParen, LeftParen, Ident("x"), RightParen, RightParen))
    )
  }

  test("Empty and boundary inputs") {
    assertEquals(lex(""), Right(List.empty[Token]))
    assertEquals(lex("   "), Right(List.empty[Token]))
    assertEquals(lex("\t"), Right(List.empty[Token]))
    assertEquals(lex("0"), Right(List(DoubleLiteral(0))))
    assertEquals(lex("."), Left(TokenizationInvalidNumericalValueError))
    assertEquals(lex("5."), Right(List(DoubleLiteral(5.0))))
  }

  test("Invalid characters") {
    assertEquals(lex("@"), Left(TokenizationInvalidCharacterError))
    assertEquals(lex("#"), Left(TokenizationInvalidCharacterError))
    assertEquals(lex("1 @ 2"), Left(TokenizationInvalidCharacterError))
  }

  test("Combined expressions") {
    assertEquals(lex("x + y"), Right(List(Ident("x"), Opt('+'), Ident("y"))))
    assertEquals(
      lex("(a + b) * 2"),
      Right(
        List(
          LeftParen,
          Ident("a"),
          Opt('+'),
          Ident("b"),
          RightParen,
          Opt('*'),
          DoubleLiteral(2)
        )
      )
    )
    assertEquals(
      lex("sin(1.5)"),
      Right(List(Ident("sin"), LeftParen, DoubleLiteral(1.5), RightParen))
    )
  }
}
