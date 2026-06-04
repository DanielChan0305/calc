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

  /** Helper function which checks whether the "name" is reserved for a function
    *
    * @return
    */
  def assignValueToVariable(name: String, value: Double): Either[CustomError, Double] =
    val isNameReservedForFuncs: Boolean = BuiltinFunctions.isDefinedAt(name)
    if (isNameReservedForFuncs) Left(NameResolutionVariableNameShadowsFunctionName(name))
    else
      UserDefinedVariables = UserDefinedVariables + (name -> value)
      Right(value)

  def getValueByName(name: String): Either[CustomError, Double] =
    name match
      case _ if UserDefinedVariables.isDefinedAt(name) =>
        Right(UserDefinedVariables(name))
      case _ if BuiltinConstants.isDefinedAt(name) =>
        Right(BuiltinConstants(name))
      case _ =>
        Left(NameResolutionVariableDoesntExists(name))
