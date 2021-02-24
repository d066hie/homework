package com.homework.typeclass

object TypeclassTask {

  trait HashCode[T] {
    def hash(value: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](value: A) {
    def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(value)
  }

  implicit val stringHashCode: HashCode[String] = _.hashCode
}
