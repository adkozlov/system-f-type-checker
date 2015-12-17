package ru.spbau.kozlov.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test
import ru.spbau.kozlov.typechecker.TypeChecker._
import ru.spbau.kozlov.typechecker.TypeCheckerTest._

/**
  * @author adkozlov
  */
class TypeCheckerTest {

  @Test
  def testId() {
    assertEquals(forAllX(X_TO_X_TYPE), check(xTypeAbstraction(xAbstraction(X_TYPE, X_TERM))))
  }

  @Test
  def testDouble() {
    assertEquals(DOUBLE_TYPE, check(DOUBLE))
  }

  @Test
  def testEvaluateDouble() {
    val natAssumption = TypeAssumption("Nat")
    val nat = natAssumption.variable

    val zeroAssumption = TermAssumption("zero", nat)
    val zero = zeroAssumption.variable

    val succAssumption = TermAssumption("succ", ->(nat, nat))
    val succ = succAssumption.variable

    val natContext = Context(List(natAssumption, zeroAssumption, succAssumption))

    assertEquals(nat,
      check(Application(Application(TypeApplication(DOUBLE, nat), succ), Application(succ, zero)), natContext))
  }

  @Test
  def testQuadruple() {
    assertEquals(DOUBLE_TYPE, check(QUADRUPLE))
  }

  @Test
  def testSelfApp() {
    val `type` = forAllX(X_TO_X_TYPE)
    assertEquals(->(`type`, `type`), check(xAbstraction(`type`, Application(TypeApplication(X_TERM, `type`), X_TERM))))
  }

  @Test(expected = classOf[TypeCheckerException])
  def testUnknownVariable() {
    check(X_TERM)
  }
}

object TypeCheckerTest {

  private val X_TERM = TermVariable("x")

  private val X_TYPE = TypeVariable("X")

  private val X_TO_X_TYPE = ->(X_TYPE, X_TYPE)

  private def xAbstraction: (Type, Term) => Term = Abstraction("x", _, _)

  private def xTypeAbstraction: Term => Term = TypeAbstraction("X", _)

  private def forAllX: Type => Type = ∀("X", _)

  private val DOUBLE = xTypeAbstraction(
    Abstraction("f", X_TO_X_TYPE,
      xAbstraction(X_TYPE,
        Application(TermVariable("f"), Application(TermVariable("f"), X_TERM)))))

  private val DOUBLE_TYPE = forAllX(->(X_TO_X_TYPE, X_TO_X_TYPE))

  private val QUADRUPLE = xTypeAbstraction(
    Application(TypeApplication(DOUBLE, X_TO_X_TYPE), TypeApplication(DOUBLE, X_TYPE)))
}
