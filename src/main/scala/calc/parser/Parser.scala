package calc.parser

import calc.error.*
import scala.compiletime.ops.double

/**
  * Helper function for implementing pratt parsing
  *
  * @param opt
  * @return
  */
def getOptPrec(opt: Char): (Int, Int) = 
  opt match
    case '^' => (101, 100)
    case '+' => (20, 21)
    case '-' => (20, 21)
    case '*' => (30, 31)
    case '/' => (30, 31)

def applyOpt(symb: Char)(l: Double, r: Double) : Double =
  println(s"$l $r")
  var fun: (Double, Double) => Double = symb match
    case '+' => (_ + _)
    case '-' => (_ - _)
    case '*' => (_ * _)
    case '/' => (_ / _)
    case '^' => math.pow

  fun(l, r)
  

/**
  * Implement pratt parsing on the List[Tokens] to get the numerical value of an expression
  *
  * @param tokens
  * @return
  */
def prattParsing(tokens: List[Token]): Either[CustomError, Double] = 
  
  // helper functions and variables
  var pos = 0

  var openingParam = 0

  // access the current character
  def cur: Token = if (pos < tokens.length) tokens(pos) else EndOfExpr

  println(s"Num tokens ${tokens.length}")
  // consumes the current character
  def consume: Token =
    val c = cur
    pos += 1
    c

  def parseExpr(prec: Int): Either[CustomError, Double] = 
    for 
      // lhs of the operand
      lhs <- parsePrefix()

      // apply infix operation with calculated lhs
      result <- loop(lhs, prec)
    yield
      result

  def loop(lhs: Double, prec: Int): Either[CustomError, Double] =
    cur match
      // Exists an operator with sufficient binding power
      case Opt(symb) if getOptPrec(symb)._1 >= prec =>
        // apply this operation
        consume
        for 
          acc_lhs <- parseInfix(symb, lhs)
          result <- loop(acc_lhs, prec)
        yield
          result

      // Exists an operator but not enough binding power
      case Opt(symb) => 
        Right(lhs)    

      //                       v
      // Handling cases like 2 (3)
      // acts as multiplication
      case LeftParam =>
        for
          rhs <- parseExpr(0)
          result <- loop(lhs * rhs, prec)

        yield 
          result

      // Leading )
      case RightParam =>  
        if (openingParam <= 0)
          Left(ParsingInvalidBracketSequence) 
        else 
          Right(lhs)

      // we have reached the end of the expression
      case EndOfExpr => 
        Right(lhs)

      case DoubleLiteral(_) => 
        Left(ParsingMissingOperator)

      case Ident(_) =>
        ???

      case _ =>
        Right(lhs)

  def parsePrefix(): Either[CustomError, Double] = 
    cur match
      // unary +
      case Opt('+') =>
        consume
        parseExpr(50)
    
      // unary -
      case Opt('-') =>
        consume
        parseExpr(50).map(x => -x)

      case Opt('*') | Opt('/') | Opt('^') =>
        Left(ParsingInvalidMathExpression)

      case Opt(_) =>
        Left(ParsingInvalidSymbol)

      case DoubleLiteral(value) =>
        consume
        Right(value)

      case Ident(name) => 
          ???

      // Leading (
      case LeftParam => 
        consume
        openingParam += 1
        var result = parseExpr(0)

        cur match
        case RightParam => 
            consume
            openingParam -= 1
            result
        case _ => Left(ParsingInvalidBracketSequence)        

      // Leading )
      case RightParam =>  
        Left(ParsingInvalidBracketSequence) 

      // Empty 
      case EndOfExpr => 
        Left(ParsingEmptyExpression)

        

  def parseInfix(symb: Char, lhs: Double): Either[CustomError, Double] =
    for 
      rhs <- parseExpr(getOptPrec(symb)._2)

    yield
      applyOpt(symb)(lhs, rhs)
  
  parseExpr(0)