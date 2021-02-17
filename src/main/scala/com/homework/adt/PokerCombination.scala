package com.homework.adt

sealed trait PokerCombination
object PokerCombination {
  type Rest = List[Rank]

  final case class StraightFlush(high: Rank, rest: Rest) extends PokerCombination
  final case class FourOfAKind(high: Rank, rest: Rest) extends PokerCombination
  final case class FullHouse(high: Rank, low: Rank) extends PokerCombination
  final case class Flush(rest: Rest) extends PokerCombination
  final case class Straight(rest: Rest) extends PokerCombination
  final case class ThreeOfAKind(high: Rank, rest: Rest) extends PokerCombination
  final case class TwoPairs(high: Rank, low: Rank, rest: Rest) extends PokerCombination
  final case class Pair(high: Rank, rest: Rest) extends PokerCombination
  final case class HighCard(rest: Rest) extends PokerCombination
}