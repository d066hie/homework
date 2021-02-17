package com.homework.adt

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CardSpec extends AnyFlatSpec {
  "fromString" should "allow to create value from string" in {
    val createdCard = Card.fromString("h8")
    val expectedCard = Card(Suit.Hearts, Rank.Eight)

    createdCard.isDefined shouldBe true
    createdCard.get shouldEqual expectedCard
  }

  "fromString" should "return None for incorrect value" in {
    val createdCard = Card.fromString("z8")

    createdCard.isEmpty shouldBe true
  }
}
