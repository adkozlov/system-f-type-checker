package ru.spbau.kozlov.typechecker

/**
  * @author adkozlov
  */
sealed trait Assumption

final case class TermAssumption(name: String, `type`: Type) extends Assumption {
  val variable = TermVariable(name)
}

final case class TypeAssumption(name: String) extends Assumption {
  val variable = TypeVariable(name)
}

final case class Context(private val assumptions: List[Assumption] = Nil) {

  def +(assumption: Assumption) = Context(assumption :: assumptions)

  def `type`(name: String) = assumptions.find {
    case TermAssumption(thatName, _) if thatName == name => true
    case _ => false
  } map {
    _.asInstanceOf[TermAssumption].`type`
  }
}
