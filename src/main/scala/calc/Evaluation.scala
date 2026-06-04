package calc

import calc.error.*
import parser.*

/** Evaluates the numerical value of a given expression
  *
  * @param rawExpr
  * @return
  */
def eval(rawExpr: String): Either[CustomError, Double] =
  for
    parsedTokens <- lex(rawExpr)
    ASTExpr      <- prattParsing(parsedTokens)
    value <- evaluateASTExpr(ASTExpr)
  yield value
