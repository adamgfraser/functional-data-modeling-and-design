package fdm

/**
 * Sometimes we don't want to take the time to model data precisely. For example, we might want to
 * model an email address with a string, even though most strings are not valid email addresses.
 *
 * In such cases, we can save time by using a smart constructor, which lets us ensure we model
 * only valid data, but without complicated data types.
 */
object smart_constructors {

  final case class CreditCard(name: String, number: CreditCardNumber, cvv: Short)

  val myCreditCard = CreditCard("Adam", ???, 123)

  sealed abstract case class CreditCardNumber private (value: String)

  object CreditCardNumber {
    def fromString(string: String): Option[CreditCardNumber] =
      if (string.matches("""\d{4}-\d{4}-\d{4}-\d{4}"""))
        Some(new CreditCardNumber(string) {})
      else
        None
  }

  val myStringInput = "1234-5678-9012-3456"

  CreditCardNumber.fromString(myStringInput) match {
    case Some(number) => CreditCard("Adam", number, 123)
    case None         => println("Invalid credit card number")
  }

  sealed abstract case class Email private (value: String)
  object Email {
    def fromString(email: String): Option[Email] =
      if (email.matches("""/\w+@\w+.com""")) Some(new Email(email) {}) else None
  }

  /**
   * EXERCISE 1
   *
   * Create a smart constructor for `NonNegative` which ensures the integer is always non-negative.
   */
  sealed abstract case class NonNegative private (value: Int)
  object NonNegative {
    def fromValue(value: Int): Either[DomainError.InvalidInt, NonNegative] =
      if (value < 0) Left(DomainError.InvalidInt(value))
      else Right(new NonNegative(value) {})
  }

  type ??? = Nothing

  val myNonNegative: Either[DomainError.InvalidInt, NonNegative] =
    NonNegative.fromValue(5)

  def callMyBusinessLogic(nonNegative: NonNegative): Unit =
    ???

  myNonNegative match {
    case Right(nonNegative)              => callMyBusinessLogic(nonNegative)
    case Left(DomainError.InvalidInt(n)) => ???
  }

  // Never throw an exception in your modeling
  // Return some data type that models exception
  // Option
  // Either
  // Try
  // Validation
  // ZIO

  // Use option when there is no meaningful additional information about the failure

  // Option as containing a failure with no useful information
  // Either[DomainError, A]
  // Try
  // Validation

  for {
    a <- NonNegative.fromValue(-5)
    b <- NonNegative.fromValue(-10)
  } yield ()

  sealed trait DomainError

  object DomainError {
    case class InvalidInt(value: Int)      extends DomainError
    case class InvalidEmail(email: String) extends DomainError
  }

  /**
   * EXERCISE 2
   *
   * Create a smart constructor for `Age` that ensures the integer is between 0 and 120.
   */
  sealed abstract case class Age private (value: Int)
  object Age {
    def fromValue(value: Int): Option[Age] =
      if (value >= 0 && value <= 120)
        Some(new Age(value) {})
      else
        None
  }

  /**
   * EXERCISE 3
   *
   * Create a smart constructor for password that ensures some security considerations are met.
   */
  sealed abstract case class Password private (value: String)
}

object applied_smart_constructors {

  /**
   * EXERCISE 1
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class BankAccount(id: String, name: String, balance: Double, opened: java.time.Instant)

  /**
   * EXERCISE 2
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class Person(age: Int, name: String, salary: Double)

  /**
   * EXERCISE 3
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class SecurityEvent(machine: String, timestamp: String, eventType: Int)
  object EventType {
    val PortScanning    = 0
    val DenialOfService = 1
    val InvalidLogin    = 2
  }
}
