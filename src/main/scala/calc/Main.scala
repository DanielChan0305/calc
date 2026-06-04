package calc

import calc.config.Constants
import calc.error.CustomError
import calc.parser.{lex, Token}
import scala.annotation.tailrec
import calc.parser.printListTokens
import calc.parser.prattParsing

@main
def main(): Unit =
  // main event loop
  var rawExpr: String = scala.io.StdIn.readLine("> ")

  while (rawExpr != "exit" && rawExpr != "quit")
    val result = eval(rawExpr)

    result match
      case Left(error) =>
        println(s"Error: ${error.message}")
      case Right(value) =>
        println(value)

    rawExpr = scala.io.StdIn.readLine("> ")

  // user quits the program
  println(s"Thanks for using ${Constants.AppName}. Have a nice day.")
