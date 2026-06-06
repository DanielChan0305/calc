package calc

import calc.error.CustomError
import parser.{lex, prattParsing, evaluateASTExpr}

/** Evaluates the numerical value of a given expression
  * Entire pipeline : Lexing -> Parsing -> Building AST -> Evaluating AST
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
