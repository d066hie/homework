package com.homework.basics

import Calculator._

/** Simple calculator with buttons.
  *
  * @param memory whatever is stored in the memory.
  * @param screen whatever you see on the screen.
  */
case class Calculator(
    memory: Int = 0,
    screen: Int = 0,
    operation: Option[Operation] = None
) {

  def enter(digit: Int): Either[String, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(this.copy(screen = screen * 10 + digit))
    } else {
      Left("digit out of range")
    }

  def plus: Calculator = this.copy(
    memory = this.screen,
    screen = 0,
    operation = Some(Operation.Plus)
  )
  def minus: Calculator = this.copy(
    memory = this.screen,
    screen = 0,
    operation = Some(Operation.Minus)
  )

  def calculate: Calculator = operation.fold(this) {
    case Operation.Plus  => Calculator(screen = memory + screen)
    case Operation.Minus => Calculator(screen = memory - screen)
  }

}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
  }
}
