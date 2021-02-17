package com.homework.adt

sealed trait Hand
object Hand {
  final case class TexasHoldem(c1: Card, c2: Card) extends Hand
  final case class OmahaHoldem(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) extends Hand
}
