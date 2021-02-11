package com.homework.adt

sealed trait TestCase
object TestCase {
  final class TexasHoldemCase(board: Board, hands: List[Hand]) extends TestCase
  final class OmahaHoldemCase(board: Board, hands: List[Hand]) extends TestCase
}
