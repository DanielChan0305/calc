package calc.parser

import calc.error.*

object IdentifierTable:
  val BuiltinConstants: Map[String, Double] = Map(
    "pi" -> math.Pi,
    "e"  -> math.E
  )

  var UserVariables: Map[String, Double] = Map()

  val BuiltinFunctions: Map[String, (Double) => Double] = Map(
    "sin" -> math.sin,
    "cos" -> math.cos
    // ("log" -> )
  )

  def isBuiltinConstant(name: String): Boolean = BuiltinConstants.isDefinedAt(name)
  def isUserVariables(name: String): Boolean = UserVariables.isDefinedAt(name)
  def isBuiltinFunc(name: String): Boolean = BuiltinFunctions.isDefinedAt(name)

  /**
    * Helper function which assigns numeric value to variable with a given name.
    * 
    * Returns error is the name collides with a builtin function.
    *
    */
  def assignValueToVariable(name: String, value: Double): Either[CustomError, Double] =
    if (isBuiltinFunc(name)) Left(ParsingInvalidUsesofFunctions(name))
    
    else
      UserVariables = UserVariables + (name -> value)
      Right(value)

  def getValueByName(name: String): Either[CustomError, Double] =
    name match
      case _ if isBuiltinFunc(name) =>
        Left(ParsingInvalidUsesofFunctions(name))

      case _ if isUserVariables(name) =>
        Right(UserVariables(name))

      case _ if isBuiltinConstant(name) =>
        Right(BuiltinConstants(name))

      case _ =>
        Left(NameResolutionVariableDoesntExists(name))
