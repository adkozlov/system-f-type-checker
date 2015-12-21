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
    val top = forAllX(X_TO_X_TYPE)
    assertEquals(->(top, top), check(xAbstraction(top, Application(TypeApplication(X_TERM, top), X_TERM))))
  }

  @Test
  def testSelfApp2() {
    val bottom = forAllX(X_TYPE)
    val bottomToBottomType = ->(bottom, bottom)
    assertEquals(bottomToBottomType, check(
      xAbstraction(bottom,
        Application(
          TypeApplication(X_TERM, bottomToBottomType),
          X_TERM))))
  }

  @Test
  def testSelfApp3() {
    val bottom = ∀("Y", ->(TypeVariable("Y"), TypeVariable("Y")))
    assertEquals(->(bottom, bottom), check(
      xAbstraction(
        bottom,
        xTypeAbstraction(
          Application(
            TypeApplication(X_TERM, X_TO_X_TYPE),
            TypeApplication(X_TERM, X_TYPE))))))
  }

  @Test
  def testSelfApp4() {
    val top = ∀("Y", ->(TypeVariable("Y"), TypeVariable("Y")))
    assertEquals(->(top, top), check(
      xAbstraction(
        top,
        xTypeAbstraction(
          Application(
            TypeApplication(X_TERM, X_TO_X_TYPE),
            TypeApplication(X_TERM, X_TYPE))))))
  }

  @Test(expected = classOf[TypeCheckerException])
  def testUnknownVariable() {
    check(X_TERM)
  }

  @Test
  def testBottom() {
    val bottom = forAllX(X_TYPE)
    val f = TermVariable("f")
    assertEquals(->(bottom, bottom), check(
      Abstraction(
        "f",
        bottom,
        Application(
          Application(
            TypeApplication(f, ->(bottom, ->(bottom, bottom))),
            TypeApplication(f, bottom)),
          TypeApplication(f, bottom)))))
  }

  @Test
  def testBottom2() {
    val bottom = forAllX(X_TYPE)
    val f = TermVariable("f")
    assertEquals(->(bottom, bottom), check(
      Abstraction(
        "f",
        bottom,
        Application(
          Application(
            TypeApplication(f, ->(->(bottom, bottom), ->(bottom, bottom))),
            TypeApplication(f, ->(bottom, bottom))),
          TypeApplication(f, ->(bottom, bottom))))))
  }

  @Test
  def testInference() {
    val yType = TypeVariable("Y")
    val term = TypeApplication(
      xTypeAbstraction(
        TypeAbstraction(
          "Y",
          xAbstraction(
            X_TYPE,
            Abstraction("y", yType, X_TERM)))),
      yType)
    assertEquals(∀("Y", ->(yType, ->(yType, yType))), check(term, Context(List(TypeAssumption("Y")))))
  }

  @Test
  def testInference2() {
    val yType = TypeVariable("Y")
    val term = Application(
      TypeApplication(
        xTypeAbstraction(
          xAbstraction(
            X_TYPE,
            TypeAbstraction(
              "Y",
              Abstraction("y", yType, X_TERM)))),
        yType),
      TermVariable("y"))
    assertEquals(∀("Y", ->(yType, yType)), check(term, Context(List(TypeAssumption("Y"), TermAssumption("y", yType)))))
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
