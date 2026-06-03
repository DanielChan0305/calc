package calc.error

sealed trait CustomError:
  def message: String

case object UnexpectedError extends CustomError:
  def message = "Unexpected Error occurred."

case object TokenizationInvalidNumericalValueError extends CustomError:
  def message = "Invalid numerical values detected. \n" +
    "Integers should only consists of digits and decimals should only contain one dot."

case object TokenizationInvalidCharacterError extends CustomError:
  def message = "Invalid character detected."

case object ParsingInvalidMathExpression extends CustomError:
  def message = "Invalid mathematical Expression. Unable to evaluate value."

case object ParsingMissingOperator extends CustomError:
  def message = "Missing operator."

case object ParsingInvalidBracketSequence extends CustomError:
  def message = "Invalid bracket sequence."

case object ParsingEmptyExpression extends CustomError:
  def message = "Empty Expression."

case object ParsingInvalidSymbol extends CustomError:
  def message = "Invalid mathematical operator."