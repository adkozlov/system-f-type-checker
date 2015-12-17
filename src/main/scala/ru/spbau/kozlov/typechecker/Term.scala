package ru.spbau.kozlov.typechecker

/**
  * @author adkozlov
  */
sealed trait Term

final case class TermVariable(name: String) extends Term {
  override def toString = name
}

final case class Abstraction(name: String, `type`: Type, term: Term) extends Term {
  override def toString = s"λ$name: ${`type`}.$term"
}

final case class Application(first: Term, second: Term) extends Term {
  override def toString = s"$first $second"
}

final case class TypeAbstraction(name: String, term: Term) extends Term {
  override def toString = s"λ$name: *.$term"
}

final case class TypeApplication(term: Term, `type`: Type) extends Term {
  override def toString = s"$term [${`type`}]"
}
