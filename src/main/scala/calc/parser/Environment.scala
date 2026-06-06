package calc.parser

import calc.error.*

object IdentifierTable:
  val BuiltinConstants: Map[String, Double] = Map(
    "pi" -> math.Pi,
    "e"  -> math.E
  )

  var UserDefinedVariables: Map[String, Double] = Map()

  val BuiltinFunctions: Map[String, (Double) => Double] = Map(
    "sin" -> math.sin,
    "cos" -> math.cos
    // ("log" -> )
  )

  def isBuiltinConstant(name: String): Boolean = BuiltinConstants.isDefinedAt(name)
  def isUserDefinedVariables(name: String): Boolean = UserDefinedVariables.isDefinedAt(name)

  /** Helper function which checks whether the "name" is reserved for a function
    *
    * @return
    */
  def isBuiltinFunc(name: String): Boolean = BuiltinFunctions.isDefinedAt(name)

  def assignValueToVariable(name: String, value: Double): Either[CustomError, Double] =
    if (isBuiltinFunc(name)) Left(ParsingInvalidUsesofFunctions(name))
    else
      UserDefinedVariables = UserDefinedVariables + (name -> value)
      Right(value)

  def getValueByName(name: String): Either[CustomError, Double] =
    name match
      case _ if isBuiltinFunc(name) =>
        Left(ParsingInvalidUsesofFunctions(name))

      case _ if isUserDefinedVariables(name) =>
        Right(UserDefinedVariables(name))

      case _ if isBuiltinConstant(name) =>
        Right(BuiltinConstants(name))

      case _ =>
        Left(NameResolutionVariableDoesntExists(name))
