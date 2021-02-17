package com.homework.adt

final case class Card(suit: Suit, rank: Rank)
object Card {
  def fromString(description: String): Option[Card] = {
    description.splitAt(1) match {
      case (suitString, rankString) =>
        (Suit.fromString(suitString), Rank.fromString(rankString)) match {
          case (None, _)                            => None
          case (_, None)                            => None
          case (Some(suit), Some(rank)) => Some(new Card(suit, rank))
        }
      case _ => None
    }
  }
}
