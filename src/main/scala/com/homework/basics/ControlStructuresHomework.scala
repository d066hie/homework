package com.homework.basics

import com.homework.basics.ControlStructuresHomework.Command._

import scala.io.Source

object ControlStructuresHomework {

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
    final case class Test(numbers: List[Double]) extends Command
  }

  sealed trait Result

  object Result {
    final case class Divide(dividend: Double, divisor: Double, result: Double) extends Result
    final case class Sum(numbers: List[Double], result: Double) extends Result
    final case class Average(numbers: List[Double], result: Double) extends Result
    final case class Min(numbers: List[Double], result: Double) extends Result
    final case class Max(numbers: List[Double], result: Double) extends Result
  }

  sealed trait FailureReason

  object FailureReason {
    final case object NotANumberInput extends FailureReason
    final case object WrongNumberOfArguments extends FailureReason
    final case object DivisionByZero extends FailureReason
    final case object UnknownCommand extends FailureReason
  }

  def parseNumbers(numbers: List[String]): Either[FailureReason, List[Double]] = {
    val doubleList: List[Option[Double]] = numbers.map(argument => argument.toDoubleOption)

    if (doubleList.exists(_.isEmpty)) {
      Left(FailureReason.NotANumberInput)
    } else {
      Right(doubleList.map(_.get))
    }
  }

  def parseCommand(line: String): Either[FailureReason, Command] = line.toLowerCase.split("\\s+").toList match {
    case command :: arguments => parseNumbers(arguments) match {
      case Right(numbers) => command match {
        case _ if numbers.isEmpty => Left(FailureReason.WrongNumberOfArguments)
        case "divide"  => numbers.length match {
          case 2 => Right(Command.Divide(numbers(0), numbers(1)))
          case _ => Left(FailureReason.WrongNumberOfArguments)
        }
        case "sum"     => Right(Command.Sum(numbers))
        case "average" => Right(Command.Average(numbers))
        case "min"     => Right(Command.Min(numbers))
        case "max"     => Right(Command.Max(numbers))
        case _         => Left(FailureReason.UnknownCommand)
      }
      case Left(reason) => Left(reason)
    }
  }

  def calculate(command: Command): Either[FailureReason, Result] = command match {
    case Divide(dividend, divisor) => divisor match {
      case 0 => Left(FailureReason.DivisionByZero)
      case _ => Right(Result.Divide(dividend, divisor, dividend / divisor))
    }
    case Sum(numbers)     => Right(Result.Sum(numbers, numbers.sum))
    case Average(numbers) => Right(Result.Average(numbers, numbers.sum / numbers.length))
    case Min(numbers)     => Right(Result.Min(numbers, numbers.min))
    case Max(numbers)     => Right(Result.Max(numbers, numbers.max))
  }

  def renderResult(result: Result): String = result match {
    case Result.Divide(dividend, divisor, result) => s"$dividend divided by $divisor is $result"
    case Result.Sum(numbers, result) => s"the sum of ${numbers.mkString(" ")} is $result"
    case Result.Average(numbers, result) => s"the average of ${numbers.mkString(" ")} is $result"
    case Result.Min(numbers, result) => s"the minimum of ${numbers.mkString(" ")} is $result"
    case Result.Max(numbers, result) => s"the maximum of ${numbers.mkString(" ")} is $result"
  }

  def renderError(error: FailureReason): String = {
    val reason = error match {
      case FailureReason.DivisionByZero => "Can't divide by zero"
      case FailureReason.UnknownCommand => "Unknown command"
      case FailureReason.NotANumberInput => "Input contains not a number"
      case FailureReason.WrongNumberOfArguments => "Wrong number of arguments for this command"
    }
    s"Error: $reason"
  }

  def process(line: String): String = {
    val answer = for {
      command <- parseCommand(line)
      result <- calculate(command)
    } yield result

    answer.fold(renderError, renderResult)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
