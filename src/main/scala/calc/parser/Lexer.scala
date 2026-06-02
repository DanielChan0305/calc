package calc.parser

import calc.error.*
import scala.annotation.tailrec

/**
 * Pretty printer for List[Tokens]
 * @param List[Tokens]
*/
def printListTokens(tokens: List[Tokens]) =
    for (token <- tokens){
        println(token)
    }      


/**
 * Lexs the raw input string into tokens
 * Returns error object is lexing was unsuccessful
 *
 * @param rawExpr
 * @return Either[CustomError, List[Tokens]]
 */
def lex(rawExpr: String): Either[CustomError, List[Tokens]] = 
    // helper functions and variables
    var pos = 0

    // access the current character
    def cur: Char = if (pos < rawExpr.length()) rawExpr(pos) else '$'

    // consumes the current character
    def consume: Char = 
        val c = cur
        pos += 1
        c
    
    // looping through rawExpr
    @tailrec
    def loop(acc: List[Tokens]) : Either[CustomError, List[Tokens]] = 
        cur match 
            // end of string
            case '$' => Right(acc.reverse)

            case ' ' | '\t' =>
                consume
                loop(acc)

            // operators
            case '+' | '-' | '*' | '/' | '^' => 
                loop(Opt(consume) :: acc)

            // left param
            case '(' =>
                loop(LeftParam :: acc)

            // right param
            case ')' => 
                loop(RightParam :: acc)        

            // variables or functions
            case _ if cur.isLetter =>
                // get the word
                var name = ""
                while (cur.isLetter || cur.isDigit) do 
                    name += consume

                loop(Ident(name) :: acc)

            // numeric values
            case _ if cur.isDigit || cur == '.' => 
                // gets the word
                var value = ""
                while (('0' <= cur && cur <= '9') || cur == '.') do
                    value += consume

                // convert to Integer
                value.toIntOption match
                    case None => 
                        // convert to Double
                        value.toDoubleOption match
                        case None => Left(TokenizationInvalidNumericalValueError)
                        case Some(value: Double) => 
                            loop(DoubleLiteral(value) :: acc)

                    // Valid Integer
                    case Some(value: Int) =>
                        loop(IntLiteral(value) :: acc)    

            case _ => 
                Left(TokenizationInvalidCharacterError)


    val res = loop(List())

    // temporarily added for seeing the lexed tokens
    res match
        case Left(error) =>
            println(error.message) 
        case Right(tokens) =>
            printListTokens(tokens)
    res

