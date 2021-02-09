package com.homework.basics

import com.homework.basics.Collections.{count, kidsWithCandies, maxWidthOfVerticalArea, maximumWealth, runningSum, scanLeft, shuffle, sortConsideringEqualValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CollectionsSpec extends AnyFlatSpec {
  "runningSum" should "provide info about sum of array on each element" in {
    runningSum(Array(3,1,2,10,1)) shouldEqual Array(3,4,6,16,17)
  }

  "shuffle" should "shuffle the giving array" in {
    shuffle(Array(2,5,1,3,4,7), 3) shouldEqual Array(2,3,5,4,1,7)
  }

  "maximumWealth" should "calculate array with the max sum" in {
    maximumWealth(
      Array(
        Array(2,8,7),
        Array(7,1,3),
        Array(1,9,5)
      )
    ) shouldEqual 17
  }

  "kidsWithCandies" should "calculate possibility to rich the max possible candies" in {
    kidsWithCandies(Array(4, 2, 1, 1, 2), 1) shouldEqual Array(true, false, false, false, false)
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
  }

  "maxWidthOfVerticalArea" should "calculate max width of possible area" in {
    maxWidthOfVerticalArea(
      Array(
        Array(8, 7),
        Array(9, 9),
        Array(7, 4),
        Array(9, 7)
      )
    ) shouldEqual 1

    maxWidthOfVerticalArea(
      Array(
        Array(3, 1),
        Array(9, 0),
        Array(1, 0),
        Array(1, 4),
        Array(5, 3),
        Array(8, 8)
      )
    ) shouldEqual 3
  }

  "count" should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  "sort considering equal values" should "be correct on example 1" in {
    val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    val obtained = sortConsideringEqualValues(input)
    obtained shouldEqual expected
  }

  it should "be correct on example 2" in {
    val values = Set("a1", "a2", "b1", "c1", "c2", "d1").map { x =>
      x -> x.head.toInt
    }.toMap

    sortConsideringEqualValues(values) shouldEqual List(
      Set("a1", "a2") -> 'a'.toInt,
      Set("b1") -> 'b'.toInt,
      Set("c1", "c2") -> 'c'.toInt,
      Set("d1") -> 'd'.toInt,
    )
  }
}
