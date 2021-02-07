package com.homework.basics

import com.homework.basics.ControlStructuresHomework.{Command, FailureReason, Result, calculate, parseCommand, parseNumbers, process, renderError, renderResult}
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ControlStructuresHomeworkSpec extends AnyFlatSpec {
  "parseNumbers" should "return left for input with a non-digit value" in {
    val result = parseNumbers(List("1", "2", "a", "4"))

    result.isRight shouldEqual false
    inside(result) { case Left(value) => value shouldEqual FailureReason.NotANumberInput }
  }

  "parseNumbers" should "return right for input with all valid values" in {
    val result = parseNumbers(List("1", "2", "3.14", "4"))

    result.isLeft shouldEqual false
    result.getOrElse(List.empty) shouldEqual List(1, 2, 3.14, 4)
  }

  "parseNumbers" should "return right for input without values" in {
    val result = parseNumbers(List())

    result.isLeft shouldEqual false
    result.getOrElse(List.empty) shouldEqual List()
  }

  "parseCommand" should "returns instance Divide for divide command" in {
    val command = parseCommand("divide 4 5")

    command.isRight shouldEqual true
    command.getOrElse(FailureReason.UnknownCommand) shouldEqual Command.Divide(4.0, 5.0)
  }

  "parseCommand" should "return FailureReason for divide command with only one argument" in {
    val command = parseCommand("divide 4")

    command.isRight shouldEqual false
    inside(command) { case Left(value) => value shouldEqual FailureReason.WrongNumberOfArguments }
  }

  "parseCommand" should "return FailureReason for any command in case missing arguments" in {
    val command = parseCommand("max ")

    command.isRight shouldEqual false
    inside(command) { case Left(value) => value shouldEqual FailureReason.WrongNumberOfArguments }
  }

  "parseCommand" should "returns FailureReason unknown command" in {
    val command = parseCommand("factorial 5")

    command.isRight shouldEqual false
    inside(command) { case Left(value) => value shouldEqual FailureReason.UnknownCommand }
  }

  "parseCommand" should "returns sum command for line with 'sum' command" in {
    val command = parseCommand("sum 5 2 4")

    command.isRight shouldEqual true
    command.getOrElse(FailureReason.UnknownCommand) shouldEqual Command.Sum(List(5, 2, 4))
  }

  "parseCommand" should "returns average command for line with 'average' command" in {
    val command = parseCommand("average 5 3 2")

    command.isRight shouldEqual true
    command.getOrElse(FailureReason.UnknownCommand) shouldEqual Command.Average(List(5, 3, 2))
  }

  "parseCommand" should "returns min command for line with 'min' command" in {
    val command = parseCommand("min 1 2 3")

    command.isRight shouldEqual true
    command.getOrElse(FailureReason.UnknownCommand) shouldEqual Command.Min(List(1, 2, 3))
  }

  "parseCommand" should "returns max command for line with 'max' command" in {
    val command = parseCommand("max 1 2 3")

    command.isRight shouldEqual true
    command.getOrElse(FailureReason.UnknownCommand) shouldEqual Command.Max(List(1, 2, 3))
  }


  "calculate" should "return FailureReason for division by zero" in {
    val result = calculate(Command.Divide(2, 0))

    result.isRight shouldEqual false
    inside(result) { case Left(value) => value shouldEqual FailureReason.DivisionByZero }
  }

  "calculate" should "calculate result for 'divide' command" in {
    val result = calculate(Command.Divide(4, 2))

    result.isRight shouldEqual true
    result.getOrElse(FailureReason.UnknownCommand) shouldEqual Result.Divide(4, 2, 2)
  }

  "calculate" should "calculate result for 'average' command" in {
    val result = calculate(Command.Average(List(2, 1, 3)))

    result.isRight shouldEqual true
    result.getOrElse(FailureReason.UnknownCommand) shouldEqual Result.Average(List(2, 1, 3), 2)
  }

  "calculate" should "calculate result for 'sum' command" in {
    val result = calculate(Command.Sum(List(2, 1, 3)))

    result.isRight shouldEqual true
    result.getOrElse(FailureReason.UnknownCommand) shouldEqual Result.Sum(List(2, 1, 3), 6)
  }

  "calculate" should "calculate result for 'min' command" in {
    val result = calculate(Command.Min(List(2, 1, 3)))

    result.isRight shouldEqual true
    result.getOrElse(FailureReason.UnknownCommand) shouldEqual Result.Min(List(2, 1, 3), 1)
  }

  "calculate" should "calculate result for 'max' command" in {
    val result = calculate(Command.Max(List(1, 3, 2)))

    result.isRight shouldEqual true
    result.getOrElse(FailureReason.UnknownCommand) shouldEqual Result.Max(List(1, 3, 2), 3)
  }

  "renderResult" should "render calculated result for Divide" in {
    renderResult(Result.Divide(4, 2, 2)) shouldEqual "4.0 divided by 2.0 is 2.0"
  }

  "renderResult" should "render calculated result for Sum" in {
    renderResult(Result.Sum(List(2, 1, 3), 6)) shouldEqual "the sum of 2.0 1.0 3.0 is 6.0"
  }

  "renderResult" should "render calculated result for Average" in {
    renderResult(Result.Average(List(2, 1, 3), 2)) shouldEqual "the average of 2.0 1.0 3.0 is 2.0"
  }

  "renderResult" should "render calculated result for Min" in {
    renderResult(Result.Min(List(2, 1, 3), 1)) shouldEqual "the minimum of 2.0 1.0 3.0 is 1.0"
  }

  "renderResult" should "render calculated result for Max" in {
    renderResult(Result.Max(List(2, 1, 3), 3)) shouldEqual "the maximum of 2.0 1.0 3.0 is 3.0"
  }

  "renderError" should "render human-readable result with Error prefix" in {
    renderError(FailureReason.UnknownCommand) shouldEqual "Error: Unknown command"
  }

  "process" should "return string which represents result of the passed command" in {
    process("divide 4 5") shouldEqual "4.0 divided by 5.0 is 0.8"
    process("sum 5 5 6 8.5") shouldEqual "the sum of 5.0 5.0 6.0 8.5 is 24.5"
    process("average 4 3 8.5 4") shouldEqual "the average of 4.0 3.0 8.5 4.0 is 4.875"
    process("min 4 -3 -17") shouldEqual "the minimum of 4.0 -3.0 -17.0 is -17.0"
    process("max 4 -3 -17") shouldEqual "the maximum of 4.0 -3.0 -17.0 is 4.0"
    process("factorial 5") shouldEqual "Error: Unknown command"
  }
}
