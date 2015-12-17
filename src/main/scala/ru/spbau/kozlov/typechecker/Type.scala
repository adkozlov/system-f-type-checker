package ru.spbau.kozlov.typechecker

/**
  * @author adkozlov
  */
sealed trait Type {

  def equalsInContext(that: Type, context: Map[String, String] = Map()): Boolean

  override def equals(that: scala.Any) = {
    that match {
      case thatType: Type => equalsInContext(thatType)
      case _ => false
    }
  }
}

final case class TypeVariable(name: String) extends Type {

  override def equalsInContext(that: Type, context: Map[String, String]) = {
    that match {
      case TypeVariable(thatName) => thatName == (context get name match {
        case Some(result) => result
        case _ => name
      })
      case _ => false
    }
  }

  override def equals(that: Any) = super.equals(that)

  override def toString = name
}

final case class ->(from: Type, to: Type) extends Type {

  override def equalsInContext(that: Type, context: Map[String, String]) = {
    that match {
      case thatFrom -> thatTo => from.equalsInContext(thatFrom, context) && to.equalsInContext(thatTo, context)
      case _ => false
    }
  }

  override def equals(that: Any) = super.equals(that)

  override def toString = s"($from -> $to)"
}

final case class ∀(name: String, `type`: Type) extends Type {

  override def equalsInContext(that: Type, context: Map[String, String]) = {
    that match {
      case ∀(thatName, thatType) => `type`.equalsInContext(thatType, context + (name -> thatName))
      case _ => false
    }
  }

  override def equals(that: Any) = super.equals(that)

  override def toString = s"∀[$name].${`type`}"
}