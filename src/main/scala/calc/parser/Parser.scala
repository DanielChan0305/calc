package calc.parser

import calc.error.*
import scala.compiletime.ops.double
import calc.parser.IdentifierTable.getValueByName

/** Helper function for implementing pratt parsing
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
    case '=' => (10, 11)

def getInfixASTNode(symb: Char)(l: Expr, r: Expr): Either[CustomError, Expr] =
  symb match
    case '=': Char => 
      l match
        case Var(name) =>
          Right(Assign(name, r))
        case _ => 
          Left(ParsingInvalidMathExpression)
      
    case '+': Char => Right(BinOpt('+', l, r))
    case '-': Char => Right(BinOpt('-', l, r))
    case '*': Char => Right(BinOpt('*', l, r))
    case '/': Char => Right(BinOpt('/', l, r))
    case '^': Char => Right(BinOpt('^', l, r))
    
  

/** Implement pratt parsing on the List[Tokens] to get the AST of an expression
  *
  * @param tokens
  * @return
  */
def prattParsing(tokens: List[Token]): Either[CustomError, Expr] =

  // helper functions and variables
  var pos = 0

  var openingParam = 0

  // access the current character
  def cur: Token = if (pos < tokens.length) tokens(pos) else EndOfExpr

  // consumes the current character
  def consume: Token =
    val c = cur
    pos += 1
    c

  // Handles the entire expression
  def parseExpr(prec: Int): Either[CustomError, Expr] =
    for
      // lhs of the operand
      lhs <- parsePrefix()
      // apply infix operation with calculated lhs
      result <- loop(lhs, prec)
    yield result

  // LHS of each expression
  def parsePrefix(): Either[CustomError, Expr] =
    cur match
      // unary +
      case Opt('+') =>
        consume
        parseExpr(50)

      // unary -
      case Opt('-') =>
        consume
        parseExpr(50).map(x => UnaryOpt('-', x))

      case Opt('*') | Opt('/') | Opt('^') | Opt('=') =>
        Left(ParsingInvalidMathExpression)

      case Opt(_) =>
        Left(ParsingInvalidSymbol)

      case DoubleLiteral(value) =>
        consume
        Right(Num(value))

      case Ident(name) =>
        // calls API from NameTable
        consume
        Right(Var(name))

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

  // Handles the infix and the rhs
  def loop(lhs: Expr, prec: Int): Either[CustomError, Expr] =
    cur match
      // Exists an operator with sufficient binding power
      case Opt(symb) if getOptPrec(symb)._1 >= prec =>
        // apply this operation
        consume
        for
          acc_lhs <- parseInfix(symb, lhs)
          result  <- loop(acc_lhs, prec)
        yield result

      // Exists an operator but not enough binding power
      case Opt(symb) =>
        Right(lhs)

      //                       v
      // Handling cases like 2 (3)
      // implicit multiplication
      case LeftParam =>
        for
          rhs    <- parseExpr(0)
          result <- loop(BinOpt('*', lhs, rhs), prec)
        yield result

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

      //
      // Handling cases like 2e
      // implicit multplication
      case Ident(_) =>
        for 
          rhs <- parseExpr(0)
          result <- loop(BinOpt('*', lhs, rhs), prec)
        yield 
          result


  def parseInfix(symb: Char, lhs: Expr): Either[CustomError, Expr] =
    for 
      rhs <- parseExpr(getOptPrec(symb)._2)
      ASTnode <- getInfixASTNode(symb)(lhs, rhs)
    yield
      ASTnode

  parseExpr(0)
