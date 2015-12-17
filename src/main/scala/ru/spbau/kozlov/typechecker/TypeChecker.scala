package ru.spbau.kozlov.typechecker

/**
  * @author adkozlov
  */
object TypeChecker {

  def check(term: Term, context: Context = Context()): Type = term match {
    case TermVariable(name) => context `type` name match {
      case Some(result) => result
      case _ => throw new TypeCheckerException(s"unknown variable `$name`")
    }
    case Abstraction(name, thatType, thatTerm) => ->(thatType, check(thatTerm, context + TermAssumption(name, thatType)))
    case Application(firstTerm, secondTerm) =>
      val firstType = check(firstTerm, context)
      val secondType = check(secondTerm, context)
      firstType match {
        case from -> to if from == secondType => to
        case _ => throw new TypeCheckerException(s"`$secondTerm: $secondType` is not applicable to `$firstTerm: $firstType`")
      }
    case TypeAbstraction(name, thatTerm) => ∀(name, check(thatTerm, context + TypeAssumption(name)))
    case TypeApplication(thatTerm, thatType) =>
      val checkedType = check(thatTerm, context)
      checkedType match {
        case ∀(name, originalType) => substitute(name, thatType)(originalType)
        case _ => throw new TypeCheckerException(s"`$thatType` is not applicable to `$term: $checkedType`")
      }
  }

  private def substitute(name: String, substitution: Type): Type => Type = {
    case argument@TypeVariable(thatName) => if (thatName == name) substitution else argument
    case ->(from, to) => ->(substitute(name, substitution)(from), substitute(name, substitution)(to))
    case argument@ ∀(thatName, thatType) => if (thatName != name) ∀(thatName, substitute(name, substitution)(thatType)) else argument
  }
}

case class TypeCheckerException(message: String) extends RuntimeException(message)
