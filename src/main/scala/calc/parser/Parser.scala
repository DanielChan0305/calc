package calc.parser

import calc.error.*
import scala.compiletime.ops.double
import calc.parser.IdentifierTable.getValueByName
import calc.parser.IdentifierTable.isBuiltinConstant
import calc.parser.IdentifierTable.isUserDefinedVariables
import calc.parser.IdentifierTable.isBuiltinFunc

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
    case '=' => (11, 10)

def getInfixASTNode(symb: Char)(l: Expr, r: Expr): Either[CustomError, Expr] =
  symb match
    case '=' =>
      l match
        case Var(name) => Right(Assign(name, r))
        case _         => Left(ParsingInvalidMathExpression)

    case '+' => Right(BinOpt('+', l, r))
    case '-' => Right(BinOpt('-', l, r))
    case '*' => Right(BinOpt('*', l, r))
    case '/' => Right(BinOpt('/', l, r))
    case '^' => Right(BinOpt('^', l, r))

/** Implement pratt parsing on the List[Tokens] to get the AST of an expression
  *
  * @param tokens
  * @return
  */
def prattParsing(tokens: List[Token]): Either[CustomError, Expr] =

  // check for empty expression
  // println(tokens.length)
  if (tokens.length == 0) {
    Left(ParsingEmptyExpression)
  } else {
    // helper functions and variables
    var pos = 0

    var openingParen = 0

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
        lhs <- parseLHS()
        // apply infix operation with calculated lhs
        result <- parseInfixAndRHS(lhs, prec)
      yield result

    // LHS of each expression
    def parseLHS(): Either[CustomError, Expr] =
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

        // Function names
        case Ident(name) if isBuiltinFunc(name) =>
          consume
          if cur == LeftParen then
            consume           // eat '('
            for
              arg <- parseExpr(0)
              _   <- if cur == RightParen then Right(consume) 
                    else Left(ParsingInvalidUsesofFunctions(name))
            yield SingleArgFunc(name, arg)
          else
            Left(ParsingInvalidUsesofFunctions(name))

        // Constant and Variables
        case Ident(name) =>
          //println("hello")
          consume
          Right(Var(name))

        // Leading (
        case LeftParen =>
          consume
          openingParen += 1

          // evaluate expression
          parseExpr(0) match
            case Left(err)        => Left(err)
            case Right(innerExpr) =>
              // check bracket matching
              if (cur == RightParen)
                consume
                openingParen -= 1
                Right(Paren(innerExpr))
              else Left(ParsingInvalidBracketSequence)

        // Leading )
        case RightParen =>
          Left(ParsingInvalidBracketSequence)

        // Empty
        case EndOfExpr =>
          Left(ParsingInvalidMathExpression)

    // Handles the infix and the rhs

    def parseInfixAndRHS(lhs: Expr, prec: Int): Either[CustomError, Expr] =
      // Infix look ahead
      cur match
        // Exists an operator with sufficient binding power
        case Opt(symb) if getOptPrec(symb)._1 >= prec =>
          // apply this operation
          consume
          for
            acc_lhs <- bindToCurrentInfix(symb, lhs)
            result  <- parseInfixAndRHS(acc_lhs, prec)
          yield result

        // Exists an operator but not enough binding power
        case Opt(symb) =>
          Right(lhs)

        //                       v
        // Handling cases like 2 (3)
        // implicit multiplication
        case LeftParen =>
          for
            rhs    <- parseExpr(0)
            result <- parseInfixAndRHS(BinOpt('*', lhs, rhs), prec)
          yield result

        // Leading )
        case RightParen =>
          Right(lhs)

        // we have reached the end of the expression
        case EndOfExpr =>
          Right(lhs)

        case DoubleLiteral(_) =>
          Left(ParsingMissingOperator)


        // Functions
        // Handling cases like 2 sin(90)
        // handling implicit multiplication
        case Ident(name) if isBuiltinFunc(name) =>
          for
            rhs    <- parseExpr(0)
            result <- parseInfixAndRHS(BinOpt('*', lhs, rhs), prec)
          yield result

        // Constant and Variables
        // Handling cases like 2e
        // implicit multplication
        case Ident(name) =>
          for
            rhs    <- parseExpr(0)
            result <- parseInfixAndRHS(BinOpt('*', lhs, rhs), prec)
          yield result

    def bindToCurrentInfix(symb: Char, lhs: Expr): Either[CustomError, Expr] =
      for
        rhs     <- parseExpr(getOptPrec(symb)._2)
        ASTnode <- getInfixASTNode(symb)(lhs, rhs)
      yield ASTnode

    parseExpr(0) match
      case Left(err) => Left(err)
      case Right(result) => 
        if (pos < tokens.length) Left(ParsingInvalidBracketSequence)
        else Right(result)
  }
