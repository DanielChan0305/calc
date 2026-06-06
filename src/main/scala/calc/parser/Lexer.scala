package calc.parser

import calc.error.CustomError
import calc.error.TokenizationInvalidCharacterError
import calc.error.TokenizationInvalidNumericalValueError
import scala.annotation.tailrec


/** Lexs the raw input string into List[Token] 
 * 
 * Returns error object is lexing was unsuccessful
  *
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
