package calc.parser

sealed trait Tokens

//Variables and function names : letter(letter|digit)*
case class Ident(name: String) extends Tokens

//Integer literals : digit*
case class IntLiteral(value: Int) extends Tokens

//Double Literals : digit*.digit*|.digit*
case class DoubleLiteral(value: Double) extends Tokens

case class Opt(symb: Char) extends Tokens
case object LeftParam      extends Tokens
case object RightParam     extends Tokens
