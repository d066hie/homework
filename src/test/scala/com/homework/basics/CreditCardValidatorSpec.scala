package com.homework.basics

import cats.syntax.all._
import cats.implicits.{catsSyntaxValidatedId, catsSyntaxValidatedIdBinCompat0}
import com.homework.basics.CreditCardValidator.CardFields.{CardNumber, ExpirationDate, Name, SecurityCode}
import com.homework.basics.CreditCardValidator.{PaymentCard, PaymentCardValidator, ValidationError}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.time.{LocalDate, Year}
import java.time.format.DateTimeFormatter

class CreditCardValidatorSpec extends AnyFlatSpec {
  "PaymentCardValidator" should "validate cards info" in {
    import ValidationError._

    val nextYear = Year.now().getValue - 1999

    val validName = "OLEG PETROV"
    val validNumber = "4111 1111 1111 1111"
    val validExpirationDate = s"12/$nextYear"
    val validSecurityCode = "123"

    PaymentCardValidator.validate(
      name = validName,
      number = validNumber,
      expirationDate = validExpirationDate,
      securityCode = validSecurityCode
    ) shouldEqual PaymentCard(
      name = Name("OLEG PETROV"),
      number = CardNumber("4111111111111111"),
      expiration = ExpirationDate(LocalDate.parse(s"20${nextYear}-12-31", DateTimeFormatter.ofPattern("yyyy-MM-dd"))),
      securityCode = SecurityCode(123)
    ).validNec

    def checkInvalid(name: String, number: String, expirationDate: String, securityCode: String, errors: Set[ValidationError]): Assertion = {
      PaymentCardValidator.validate(
        name = name,
        number = number,
        expirationDate = expirationDate,
        securityCode = securityCode
      ).leftMap(_.toList.toSet) shouldBe errors.invalid
    }

    checkInvalid(
      name = "",
      number = validNumber,
      expirationDate = validExpirationDate,
      securityCode = validSecurityCode,
      errors = Set(NameIsInvalid)
    )

    checkInvalid(
      name = validName,
      number = "4111 1a11 1111 1111",
      expirationDate = validExpirationDate,
      securityCode = validSecurityCode,
      errors = Set(CardNumberNonDigit)
    )

    checkInvalid(
      name = validName,
      number = "4111 1121 1111 1111",
      expirationDate = validExpirationDate,
      securityCode = validSecurityCode,
      errors = Set(CardNumberCheckSumFailed)
    )

    checkInvalid(
      name = validName,
      number = validNumber,
      expirationDate = "13/25",
      securityCode = validSecurityCode,
      errors = Set(WrongDate)
    )

    checkInvalid(
      name = validName,
      number = validNumber,
      expirationDate = s"12/${nextYear - 5}",
      securityCode = validSecurityCode,
      errors = Set(ExpiredCard)
    )

    checkInvalid(
      name = validName,
      number = validNumber,
      expirationDate = validExpirationDate,
      securityCode = "23",
      errors = Set(IncorrectSecurityCode)
    )
  }
}
