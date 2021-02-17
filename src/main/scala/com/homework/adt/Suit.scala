package com.homework.adt

sealed trait Suit
object Suit {
  final case object Hearts extends Suit
  final case object Diamonds extends Suit
  final case object Spades extends Suit
  final case object Clubs extends Suit

  def fromString(value: String): Option[Suit] = {
    value match {
      case "h" => Some(Hearts)
      case "d" => Some(Diamonds)
      case "s" => Some(Spades)
      case "c" => Some(Clubs)
      case _   => None
    }
  }
}
