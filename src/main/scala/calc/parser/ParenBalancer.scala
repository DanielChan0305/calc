package calc.parser

import calc.error.CustomError
import scala.annotation.tailrec
import calc.error.ParsingInvalidBracketSequence

/**
  * Takes in the rawExpression. Balances the expression by adding trailing ')' s
  * 
  * Returns error if the bracket sequence is impossible to balance.
  *
  */
def autoBalanceParen(rawExpr: String): Either[CustomError, String] = 
  @tailrec
  def loop(rawExpr: List[Char], openParen: Int): Either[CustomError, Int] = 
    rawExpr match 
      case cur::tail => 
        //println(cur)

        cur match
          case '(' => loop(tail, openParen + 1)
          case ')' => 
            if openParen <= 0 then 
              Left(ParsingInvalidBracketSequence) 
            else 
              loop(tail, openParen - 1)

          case _ => loop(tail, openParen)
      

      case List() => 
        Right(openParen)

  val numOfUnmatchedOpenParen = loop(rawExpr.toList, 0)

  numOfUnmatchedOpenParen match 
    case Left(error) => Left(error)
    case Right(value) if value < 0 =>
      Left(ParsingInvalidBracketSequence)
      
    case Right(value) if value >= 0 =>
      Right(rawExpr + ")" * value)
