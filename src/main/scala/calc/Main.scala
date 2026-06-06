package calc

import calc.config.Constants

@main
def main(): Unit =
  // main event loop
  var rawInput: String = scala.io.StdIn.readLine("> ")

  while (rawInput.trim() != "exit" && rawInput.trim() != "quit")
    val exprVal = eval(rawInput)

    exprVal match
      case Left(error) =>
        println(s"Error: ${error.message}")
      case Right(value) =>
        println(value)

    rawInput = scala.io.StdIn.readLine("> ")

  // user quits the program
  println(s"Thanks for using ${Constants.AppName}. Have a nice day.")
