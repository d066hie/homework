package com.homework.basics

import cats.data.{Validated, ValidatedNec}
import cats.implicits.{catsSyntaxOption, catsSyntaxTuple4Semigroupal}
import com.homework.basics.CreditCardValidator.ValidationError._

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, YearMonth}
import scala.util.Try

object CreditCardValidator {

  object CardFields {

    final case class Name(name: String) extends AnyVal

    final case class CardNumber(number: String) extends AnyVal

    final case class ExpirationDate(date: LocalDate) extends AnyVal

    final case class SecurityCode(code: Number) extends AnyVal

  }


  case class PaymentCard(
                          name: CardFields.Name,
                          number: CardFields.CardNumber,
                          expiration: CardFields.ExpirationDate,
                          securityCode: CardFields.SecurityCode
                        )

  sealed trait ValidationError

  object ValidationError {

    final case object NameIsInvalid extends ValidationError {
      override def toString: String = "Card holder name must contain at least 1 char"
    }

    final case object CardNumberNonDigit extends ValidationError {
      override def toString: String = "Card number contains a non-digit char"
    }

    final case object CardNumberCheckSumFailed extends ValidationError {
      override def toString: String = "It seems like some digits in card number are incorrect"
    }

    final case object WrongDate extends ValidationError {
      override def toString: String = "Provided expiration date doesn't look correct"
    }

    final case object ExpiredCard extends ValidationError {
      override def toString: String = "Provided expiration date is in past"
    }

    final case object IncorrectSecurityCode extends ValidationError {
      override def toString: String = "Format of the provided security code doesn't look correct"
    }

  }

  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = (
      validateName(name),
      validateNumber(number),
      validateExpirationDate(expirationDate),
      validateSecurityCode(securityCode)
      ).mapN(PaymentCard)

    private def validateName(name: String): AllErrorsOr[CardFields.Name] = {
      def validateNameContent: AllErrorsOr[CardFields.Name] =
        Validated.condNec(name.nonEmpty, CardFields.Name(name), NameIsInvalid)

      validateNameContent
    }

    private def validateNumber(number: String): AllErrorsOr[CardFields.CardNumber] = {
      def validateChars: AllErrorsOr[CardFields.CardNumber] = {
        val withoutSpaces = number.replaceAll("\\s", "")
        Validated.condNec(withoutSpaces.matches("^\\d+$"), CardFields.CardNumber(withoutSpaces), CardNumberNonDigit)
      }

      def validateCheckSum(cardNumber: CardFields.CardNumber): AllErrorsOr[CardFields.CardNumber] = {
        Validated.condNec(LuhnValidator.validate(cardNumber.number), cardNumber, CardNumberCheckSumFailed)
      }

      validateChars.andThen(digitsNumber => validateCheckSum(digitsNumber))
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[CardFields.SecurityCode] = {
      def validateCodeFormat: AllErrorsOr[CardFields.SecurityCode] =
        Validated.condNec(securityCode.matches("\\d{3}"), CardFields.SecurityCode(securityCode.toInt), IncorrectSecurityCode)

      validateCodeFormat
    }

    private def validateExpirationDate(date: String): AllErrorsOr[CardFields.ExpirationDate] = {
      def validateDateFormat: AllErrorsOr[CardFields.ExpirationDate] =
        DateParser.parse(date).toValidNec(WrongDate).map(CardFields.ExpirationDate)

      def validateExpiration(expirationDate: CardFields.ExpirationDate): AllErrorsOr[CardFields.ExpirationDate] =
        Validated.condNec(InPastValidator.validate(expirationDate.date), expirationDate, ExpiredCard)

      validateDateFormat.andThen(parsedDate => validateExpiration(parsedDate))
    }
  }

  private object InPastValidator {
    def validate(date: LocalDate, now: LocalDate = LocalDate.now()): Boolean = {
      date.isAfter(now)
    }
  }

  private object DateParser {
    def parse(date: String): Option[LocalDate] = {
      Try(
        YearMonth
          .from(
            LocalDate
              .parse(
                s"${date}/01",
                DateTimeFormatter.ofPattern("MM/yy/dd")
              )
          )
          .atEndOfMonth()
      ).toOption
    }
  }

  private object LuhnValidator {
    val digitalRoot = Seq(
      0, 0,
      1, 2,
      2, 4,
      3, 6,
      4, 8,
      5, 1,
      6, 3,
      7, 5,
      8, 7,
      9, 9)

    val calculateLuhnSum: String => Int = creditCardNumber => {
      creditCardNumber.map {
        _.asDigit
      }.reverse.zipWithIndex.map {
        case (digit, index) => digitalRoot(digit * 2 + index % 2)
      }.sum
    }

    def validate(creditCardNumber: String): Boolean = {
      calculateLuhnSum(creditCardNumber) % 10 == 0
    }
  }

}
