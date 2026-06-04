package calc.parser

import calc.error.*
import scala.annotation.tailrec

/** Pretty printer for List[Tokens]
  * @param List[Tokens]
  */
def printListTokens(tokens: List[Token]) =
  for (token <- tokens)
    println(token)

/** Lexs the raw input string into tokens Returns error object is lexing was unsuccessful
  *
  * @param rawExpr
  * @return
  *   Either[CustomError, List[Tokens]]
  */
def lex(rawExpr: String): Either[CustomError, List[Token]] =
  // helper functions and variables
  var pos = 0

  // access the current character
  def cur: Char = if (pos < rawExpr.length()) rawExpr(pos) else '\u0000'

  // consumes the current character
  def consume: Char =
    val c = cur
    pos += 1
    c

  // looping through rawExpr
  @tailrec
  def loop(acc: List[Token]): Either[CustomError, List[Token]] =
    cur match
      // end of string
      case '\u0000' => Right(acc.reverse)

      case ' ' | '\t' =>
        consume
        loop(acc)

      // operators
      case '+' | '-' | '*' | '/' | '^' | '=' =>
        loop(Opt(consume) :: acc)

      // left param
      case '(' =>
        consume
        loop(LeftParen :: acc)

      // right param
      case ')' =>
        consume
        loop(RightParen :: acc)

      // variables or functions
      case _ if cur.isLetter =>
        // get the word
        var name = ""
        while cur.isLetter || cur.isDigit do name += consume

        loop(Ident(name) :: acc)

      // numeric values
      case _ if cur.isDigit || cur == '.' =>
        // gets the word
        var value = ""
        while cur.isDigit || cur == '.' do value += consume

        // convert to Double
        value.toDoubleOption match
          case None                => Left(TokenizationInvalidNumericalValueError)
          case Some(value: Double) =>
            loop(DoubleLiteral(value) :: acc)

      case _ =>
        Left(TokenizationInvalidCharacterError)

  loop(List())
