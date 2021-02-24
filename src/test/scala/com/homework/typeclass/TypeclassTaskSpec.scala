package com.homework.typeclass

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import com.homework.typeclass.TypeclassTask.HashCodeSyntax

class TypeclassTaskSpec extends AnyFlatSpec {
  "stringHashCode" should "provide a method to receives object's hash" in {
    val hello = "world"

    hello.hash shouldEqual hello.hashCode
  }
}
