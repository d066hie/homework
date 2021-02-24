package com.homework.typeclass

object Homework {

  object Task1 {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = _.amount compare _.amount
  }

  object Task2 {

    trait Show[T] {
      def show(entity: T): String
    }

    object Show {
      def apply[A](implicit instance: Show[A]): Show[A] = instance
    }

    implicit class ShowSyntax[A](value: A) {
      def show(implicit show: Show[A]): String = show.show(value)
    }

    final case class User(id: String, name: String)

    implicit val userShow: Show[User] = _.toString
  }

  object Task3 {
    type Error = String

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    object Parse {
      def apply[A](implicit instance: Parse[A]): Parse[A] = instance
    }

    implicit class ParseSyntax[A](value: String) {
      def parse(implicit parse: Parse[A]): Either[Error, A] = parse.parse(value)
    }

    final case class User(id: String, name: String)

    implicit val userParse: Parse[User] = value =>
      value.split(",").toList match {
        case id :: name :: Nil => Right(User(id, name))
        case _ => Left("Can't cast this string to a User")
      }
  }

  object Task4 {

    trait TypesafeEquals[T] {
      def eql(entity1: T, entity2: T): Boolean
    }

    object TypesafeEquals {
      def apply[A: TypesafeEquals]: TypesafeEquals[A] = implicitly
    }

    implicit class TypesafeEqualsSyntax[A: TypesafeEquals](entity1: A) {
      def ====(entity2: A): Boolean = TypesafeEquals[A].eql(entity1, entity2)
    }

    implicit val boolTypesafeEquals: TypesafeEquals[Boolean] = _ == _
  }

  object AdvancedHomework {

    trait FlatMap[F[_]] {
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

  }

}
