package com.homework.basics

import Calculations._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CalculationsSpec extends AnyFlatSpec {
  "gcd" should "calculate GCD" in {
    gcd(54, 24) shouldEqual 6
    gcd(462, 1071) shouldEqual 21
  }

  "gcd" should "return 1 for co-prime numbers" in {
    gcd(7, 10) shouldEqual 1
  }

  "gcd" should "return non-zero value if one of the arguments is zero" in {
    gcd(0, 10) shouldEqual 10
    gcd(10, 0) shouldEqual 10
  }

  "lcm" should "calculate LCM" in {
    lcm(6, 10) shouldEqual 30
    lcm(6, 21) shouldEqual 42
  }

  "lcm" should "raise an exception if any value is 0" in {
    intercept[IllegalArgumentException] { lcm(0, 5) }
    intercept[IllegalArgumentException] { lcm(5, 0) }
  }

  "lcm" should "return positive LCM even for negative arguments" in {
    lcm(-6, 10) shouldEqual 30
    lcm(6, -10) shouldEqual 30
  }
}
