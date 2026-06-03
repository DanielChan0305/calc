package calc.parser

sealed trait Token

//Variables and function names : letter(letter|digit)*
case class Ident(name: String) extends Token


// All calculations are done in double
// Double Literals : digit*|digit*.digit*|.digit*
case class DoubleLiteral(value: Double) extends Token

case class Opt(symb: Char) extends Token
case object LeftParam      extends Token
case object RightParam     extends Token

case object EndOfExpr extends Token