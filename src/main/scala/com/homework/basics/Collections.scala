package com.homework.basics

object Collections {
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val (xs, ys) = nums.splitAt(n)
    xs.lazyZip(ys).flatMap(Array(_, _))
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xs = points.map(_.head).sorted
    xs.lazyZip(xs.tail).map((x1, x2) => x2 - x1).max
  }

  def count(s: String): List[(Char, Int)] = {
    s match {
      case _ if s.isEmpty => Nil
      case str =>
        (str.head -> str.takeWhile(_ == str.head).length) :: count(str.dropWhile(_ == str.head))
    }
  }

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list.foldLeft(List(zero)) { (acc, value) =>
      f(acc.head, value) :: acc
    }.reverse
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    map
      .groupBy { case (_key, value) => value }
      .view
      .mapValues(_.keys.toSet)
      .toList
      .map { case (count, set) => (set -> count) }
  }
}
