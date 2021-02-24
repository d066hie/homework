package com.homework.typeclass

import com.homework.typeclass.Homework.{Task1, Task2}
import com.homework.typeclass.Homework.Task3.ParseSyntax
import com.homework.typeclass.Homework.Task4.TypesafeEqualsSyntax
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class HomeworkSpec extends AnyFlatSpec with Matchers {
  "Task1" should "allow to order Money objects" in {
    val money1 = Task1.Money(amount = 25)
    val money2 = Task1.Money(amount = 50)

    Array(money2, money1).sorted shouldEqual Array(money1, money2)
  }

  "Task2" should "allow to show User in a funny way" in {
    val user = Task2.User(id = "42", name = "Igor")

    user.show shouldEqual "User(42,Igor)"
  }

  "Task3" should "allow getting a User from string" in {
    val correctUser = "42,Igor"
    val result = correctUser.parse

    result.isRight shouldBe true
    inside(result) { case Right(value) => {
      value.id shouldEqual "42"
      value.name shouldEqual "Igor"
    }
    }

    val incorrectUser = "42____Igor"
    incorrectUser.parse.isLeft shouldBe true
  }

  "Task4" should "allow typesafe comparing with ===" in {
    (true ==== true) shouldBe true
    "true ==== false" should compile
    "true ==== 1" shouldNot compile
  }
}
