package com.homework.adt

sealed trait Rank
object Rank {
  final case object Two extends Rank
  final case object Three extends Rank
  final case object Four extends Rank
  final case object Five extends Rank
  final case object Six extends Rank
  final case object Seven extends Rank
  final case object Eight extends Rank
  final case object Nine extends Rank
  final case object Ten extends Rank
  final case object Jack extends Rank
  final case object Queen extends Rank
  final case object King extends Rank
  final case object Ace extends Rank

  def fromString(value: String): Option[Rank] = {
    value match {
      case "2" => Some(Two)
      case "3" => Some(Three)
      case "4" => Some(Four)
      case "5" => Some(Five)
      case "6" => Some(Six)
      case "7" => Some(Seven)
      case "8" => Some(Eight)
      case "9" => Some(Nine)
      case "T" => Some(Ten)
      case "J" => Some(Jack)
      case "Q" => Some(Queen)
      case "K" => Some(King)
      case "A" => Some(Ace)
      case _   => None
    }
  }
}
