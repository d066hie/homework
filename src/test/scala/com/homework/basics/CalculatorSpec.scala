package com.homework.basics

import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object Helper extends EitherValues {
  def digits(number: Int): List[Int] = {
    if (number < 10) {
      List(number)
    } else {
      digits(number / 10) ++ List(number % 10)
    }
  }

  def enter(start: Calculator, number: Int): Calculator = {
    digits(number).foldLeft(start) {
      case (calculator, digit) => {
        calculator.enter(digit).right.value
      }
    }
  }
}

class CalculatorSpec extends AnyFunSuite with EitherValues {
  test("stores 0 by default") {
    val calculator = Calculator()
    assert(calculator.screen == 0)
  }

  test("doesn't allow entering non-numeral") {
    val calculator = Calculator()
    assert(calculator.enter(123).left.value == "digit out of range")
  }

  test("allows entering numbers") {
    val calculator = Calculator()
    assert(calculator.enter(5).right.value == Calculator(0, 5, None))
    assert(
      calculator
        .enter(1)
        .right
        .value
        .enter(2)
        .right
        .value
        .enter(3)
        .right
        .value == Calculator(0, 123, None)
    )
    assert(Helper.enter(calculator, 123) == Calculator(0, 123, None))
  }

  test("allows adding numbers") {
    val calculator = Calculator()
    val result = Helper.enter(Helper.enter(calculator, 123).plus, 123).calculate

    assert(result == Calculator(0, 246, None))
  }

  test("allows subtracting numbers") {
    val calculator = Calculator()
    val result =
      Helper.enter(Helper.enter(calculator, 123).minus, 123).calculate

    assert(result == Calculator(0, 0, None))
  }
}

object CalculatorSpecification extends Properties("Calculator") {
  val digitsGen = for {
    a <- Gen.choose(1, 100)
    b <- Gen.choose(1, 100)
  } yield (a, b)

  property("minus") = forAll(digitsGen) {
    case (a, b) => {
      val calculator = Calculator()
      val result = Helper.enter(Helper.enter(calculator, a).minus, b).calculate
      result == Calculator(0, a - b, None)
    }
  }

  property("plus") = forAll(digitsGen) {
    case (a, b) => {
      val calculator = Calculator()
      val result = Helper.enter(Helper.enter(calculator, a).plus, b).calculate
      result == Calculator(0, a + b, None)
    }
  }
}
