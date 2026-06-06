package calc.parser

import calc.error.*
import scala.compiletime.ops.double
import calc.parser.IdentifierTable.getValueByName
import calc.parser.IdentifierTable.isBuiltinConstant
import calc.parser.IdentifierTable.isUserVariables
import calc.parser.IdentifierTable.isBuiltinFunc

/**
  * Operator precedence for binary operators
  * e.g. 2 + 5; 6 - 7 
  *
  */
def getInfixOptPrec(opt: Char): (Int, Int) =
  opt match
    case '=' => (11, 10)
    case '+' => (20, 21)
    case '-' => (20, 21)
    case '*' => (30, 31)
    case '/' => (30, 31)
    case '^' => (101, 100)

/**
  * Operator precedence for unary operators
  *
  */
def getUnaryOptPrec(opt: Char): Int = 
  opt match
    case '+' => 50
    case '-' => 50

def getASTNode(symb: Char)(l: Expr, r: Expr): Either[CustomError, Expr] =
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
    // helper functions and variables
    var pos = 0
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
        result <- parseInfixAndRhs(lhs, prec)
      yield result

    // LHS of each expression
    def parseLHS(): Either[CustomError, Expr] =
      cur match
        // unary +
        case Opt('+') =>
          consume
          parseExpr(getUnaryOptPrec('+'))

        // unary -
        case Opt('-') =>
          consume
          parseExpr(getUnaryOptPrec('-')).map(x => UnaryOpt('-', x))

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
          if (cur == LeftParen) {
            consume           // eat '('
            for
              arg <- parseExpr(0)
              _   <- if (cur == RightParen) Right(consume) 
                    else Left(ParsingInvalidUsesofFunctions(name))

            yield 
              SingleArgFunc(name, arg)
          }else{
            Left(ParsingInvalidUsesofFunctions(name))
          }

        // Constant and Variables
        case Ident(name) =>
          //println("hello")
          consume
          Right(Var(name))

        // Leading (
        case LeftParen =>
          consume

          // evaluate expression
          parseExpr(0) match
            case Left(err)        => Left(err)
            case Right(innerExpr) =>
              // check bracket matching
              if (cur == RightParen)
                consume
                Right(Paren(innerExpr))
              else 
                Left(ParsingInvalidBracketSequence)

        // Leading )
        case RightParen =>
          Left(ParsingInvalidBracketSequence)

        // Empty
        case EndOfExpr =>
          Left(ParsingInvalidMathExpression)

    // Handles the infix and the rhs

    def parseInfixAndRhs(lhs: Expr, prec: Int): Either[CustomError, Expr] =
      cur match
        // Exists an operator with sufficient binding power
        case Opt(symb) if getInfixOptPrec(symb)._1 >= prec =>
          // apply this operation
          consume
          for
            // after each infix binds successfully it will trigger the next infix
            accLhs <- bindLhsToInfix(symb, lhs)
            result  <- parseInfixAndRhs(accLhs, prec)
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
            result <- parseInfixAndRhs(BinOpt('*', lhs, rhs), prec)
          yield result

        // Functions
        // Handling cases like 2 sin(90)
        // handling implicit multiplication
        case Ident(name) if isBuiltinFunc(name) =>
          for
            rhs    <- parseExpr(0)
            result <- parseInfixAndRhs(BinOpt('*', lhs, rhs), prec)
          yield result

        // Constant and Variables
        // Handling cases like 2e
        // implicit multplication
        case Ident(name) =>
          for
            rhs    <- parseExpr(0)
            result <- parseInfixAndRhs(BinOpt('*', lhs, rhs), prec)
          yield result

        // We have reached the end of a scope
        case RightParen =>
          Right(lhs)

        // we have reached the end of the expression
        case EndOfExpr =>
          Right(lhs)

        case DoubleLiteral(_) =>
          Left(ParsingMissingOperator)

    def bindLhsToInfix(symb: Char, lhs: Expr): Either[CustomError, Expr] =
      for
        // get the right handside of the expression
        rhs     <- parseExpr(getInfixOptPrec(symb)._2)
        ASTnode <- getASTNode(symb)(lhs, rhs)
      yield ASTnode

    // entry point
    if (tokens.length == 0) {
      Left(ParsingEmptyExpression)
    }else{
      parseExpr(0) match
        case Left(error) => Left(error)
        case Right(evalVal) => 
          // Handle trailing )
          if (pos < tokens.length) Left(ParsingInvalidBracketSequence)
          else Right(evalVal)
    }