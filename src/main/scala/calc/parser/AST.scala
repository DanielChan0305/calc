package calc.parser

import calc.error.CustomError
import calc.parser.IdentifierTable.getValueByName
import calc.error.ParsingInvalidMathExpression
import calc.error.ParsingInvalidFunctionName
import calc.parser.IdentifierTable.assignValueToVariable

sealed trait Expr

/**
  * Types of AST nodes
  *
  */

case class Num(value: Double)                      extends Expr
case class Var(name: String)                       extends Expr
case class Paren(expr: Expr)                       extends Expr

case class BinOpt(opt: Char, lhs: Expr, rhs: Expr) extends Expr:
  def apply(l: Double, r: Double): Either[CustomError, Double] =
    opt match
      case '+' => Right(l + r)
      case '-' => Right(l - r)
      case '*' => Right(l * r)
      case '/' => Right(l / r)
      case '^' => Right(math.pow(l, r))
      case _   => Left(ParsingInvalidMathExpression)

case class UnaryOpt(opt: Char, expr: Expr)         extends Expr:
  def apply(exprVal: Double): Either[CustomError, Double] =
    opt match
      case '-' => Right(-1.0 * exprVal)
      case '+' => Right(exprVal)
      case _   => Left(ParsingInvalidMathExpression)


case class SingleArgFunc(name: String, param: Expr) extends Expr:
  def apply(param: Double): Either[CustomError, Double] =
    name match
      case "sin" => Right(math.sin(param))
      case _     => Left(ParsingInvalidFunctionName(name))
    

case class Assign(name: String, expr: Expr)        extends Expr


/**
  * Evaluating the AST Expression into getting the actual numerical answer.
  *
  * @param ASTExpr
  * @return
  */
def evaluateASTExpr(ASTExpr: Expr): Either[CustomError, Double] =
  ASTExpr match
    case Num(value)            => Right(value)
    case Var(name)             => getValueByName(name)
    case binOp @ BinOpt(_, lhs, rhs) =>
      for
        lhsVal <- evaluateASTExpr(lhs)
        rhsVal <- evaluateASTExpr(rhs)
        result  <- binOp.apply(lhsVal, rhsVal)
      yield result

    case unaryOpt @ UnaryOpt(opt, expr) =>
      for
        exprVal <- evaluateASTExpr(expr)
        result <- unaryOpt.apply(exprVal)
      yield result


    case singleArgFunc @ SingleArgFunc(name, param) =>
      for
        paramVal  <- evaluateASTExpr(param)
        result <- singleArgFunc.apply(paramVal)
      yield result

    case Assign(name, expr) =>
      for
        exprVal <- evaluateASTExpr(expr)
        result   <- assignValueToVariable(name, exprVal)
      yield result

    case Paren(expr) =>
      evaluateASTExpr(expr)
