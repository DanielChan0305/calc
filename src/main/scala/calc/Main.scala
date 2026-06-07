package calc

import calc.config.Constants
import org.jline.reader.{LineReaderBuilder}
import org.jline.terminal.{TerminalBuilder}
import org.jline.reader.impl.completer.StringsCompleter
import calc.parser.IdentifierTable.getBuiltinIdentifiers

@main
def main(): Unit =
  // main event loop
  val terminal = TerminalBuilder.builder().build()
  val completer = StringsCompleter(getBuiltinIdentifiers().toSeq*);
  val reader = LineReaderBuilder.builder()
                                .terminal(terminal)
                                .completer(completer)
                                .build()

  var rawInput: String = reader.readLine("> ")

  while (rawInput.trim() != "exit" && rawInput.trim() != "quit")
    val exprVal = eval(rawInput)

    exprVal match
      case Left(error) =>
        println(s"Error: ${error.message}")
      case Right(value) =>
        println(value)

    rawInput = reader.readLine("> ")

  // user quits the program
  println(s"Thanks for using ${Constants.AppName}. Have a nice day.")
