package com.homework.basics

import scala.annotation.tailrec

object Calculations {
  def lcm(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      throw new IllegalArgumentException("Can't calculate LCM for a zero value")
    }
    Math.abs(a * b / gcd(a, b))
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
  }
}
