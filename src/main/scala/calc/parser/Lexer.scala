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
  // looping through rawExpr
  @tailrec
  def loop(chars: List[Char], acc: List[Token]): Either[CustomError, List[Token]] =
    chars match
      // end of string
      case Nil => Right(acc.reverse)

      // whitespace
      case (' ' | '\t') :: tail =>
        loop(tail, acc)

      // operators
      case (op @ ('+' | '-' | '*' | '/' | '^' | '=')) :: tail =>
        loop(tail, Opt(op) :: acc)

      // left param
      case '(' :: tail =>
        loop(tail, LeftParen :: acc)

      // right param
      case ')' :: tail =>
        loop(tail, RightParen :: acc)

      // variables or functions
      case head :: tail if head.isLetter =>
        val (nameChars, rest) = tail.span(c => c.isLetter || c.isDigit)
        loop(rest, Ident((head :: nameChars).mkString) :: acc)

      // numeric values
      case head :: tail if head.isDigit || head == '.' =>
        val (valueChars, rest) = tail.span(c => c.isDigit || c == '.')
        (head :: valueChars).mkString.toDoubleOption match
          case None        => Left(TokenizationInvalidNumericalValueError)
          case Some(value) => loop(rest, DoubleLiteral(value) :: acc)

      case _ :: _ =>
        Left(TokenizationInvalidCharacterError)

  loop(rawExpr.toList, List())
