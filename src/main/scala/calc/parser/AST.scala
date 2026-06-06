package calc.parser

import calc.error.CustomError
import calc.parser.IdentifierTable.getValueByName
import calc.error.ParsingInvalidMathExpression
import calc.error.ParsingInvalidFunctionName
import calc.parser.IdentifierTable.assignValueToVariable

sealed trait Expr

case class Num(value: Double)                      extends Expr
case class Var(name: String)                       extends Expr
case class Paren(expr: Expr)                       extends Expr

case class BinOpt(opt: Char, lhs: Expr, rhs: Expr) extends Expr
case class UnaryOpt(opt: Char, expr: Expr)         extends Expr

case class singleArgFunc(name: String, param1: Expr) extends Expr

case class Assign(name: String, expr: Expr)        extends Expr

def applyBinOpt(symb: Char)(l: Double, r: Double): Double =
  var fun: (Double, Double) => Double = symb match
    case '+' => (_ + _)
    case '-' => (_ - _)
    case '*' => (_ * _)
    case '/' => (_ / _)
    case '^' => math.pow

  fun(l, r)

def evaluateASTExpr(ASTExpr: Expr): Either[CustomError, Double] =
  ASTExpr match
    case Num(value)            => Right(value)
    case Var(name)             => getValueByName(name)
    case BinOpt(opt, lhs, rhs) =>
      for
        lhs_val <- evaluateASTExpr(lhs)
        rhs_val <- evaluateASTExpr(rhs)

        result <- Right(applyBinOpt(opt)(lhs_val, rhs_val))
      yield result

    case UnaryOpt(opt, expr) =>
      for
        expr_val <- evaluateASTExpr(expr)
        result   <- opt match
          case '-' => Right(-1.0 * expr_val)
          case '+' => Right(expr_val)
          case _   => Left(ParsingInvalidMathExpression)
      yield result


    case singleArgFunc(name, param) =>
      for
        value  <- evaluateASTExpr(param)
        result <- name match
          case "sin" => Right(math.sin(value))
          case _     => Left(ParsingInvalidFunctionName(name))
      yield result
        

    case Assign(name, expr) =>
      for
        expr_val <- evaluateASTExpr(expr)
        result   <- assignValueToVariable(name, expr_val)
      yield result

    case Paren(expr) =>
      evaluateASTExpr(expr)
