package calc

import calc.config.Constants
import calc.error.CustomError
import calc.parser.{lex, Tokens}
import calc.scala.annotation.tailrec
import calc.parser.printListTokens

@main 
def main(): Unit =
    // main event loop
    var rawExpr: String = scala.io.StdIn.readLine("> ")

    while (rawExpr != "exit" && rawExpr != "quit")
        eval(rawExpr)
        rawExpr = scala.io.StdIn.readLine("> ")

    // user quits the program
    println(s"Thanks for using ${Constants.AppName}. Have a nice day.")

def eval(rawExpr: String) : Either[CustomError, Double] = 
    for 
        parsedTokens <- lex(rawExpr)

    yield 
        0.0    
