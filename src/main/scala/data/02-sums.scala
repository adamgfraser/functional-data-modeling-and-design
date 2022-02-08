package fdm

import java.util.Currency

/**
 * Scala supports eithers, which are a generic way to store one of two or more pieces of information at
 * the same time. For example, the either `Either[Boolean, Double]` stores either a `Boolean`, or
 * a `Double`.
 *
 * Eithers come with lots of free functionality, including constructors, deconstructors, equality
 * and hash code, and string representation for debugging.
 *
 * Because eithers can be one of two values, it is necessary to use pattern matching in order to
 * figure out which of the two possible types an either value contains.
 *
 * Like tuples, eithers are immutable: once constructed, they cannot change.
 *
 * Eithers are examples of "anonymous sums": they are types formed using "sum composition" of other
 * types.
 */
object eithers {

  // Either an A or a B (but not both, and always at least one)
  // North South East West

  // product types
  // anonymous ones - tuples
  // user defined ones - case classes

  // sum types
  // anonymous ones - either
  // user defined ones - sealed trait with subtypes

  /**
   * EXERCISE 1
   *
   * Using both a type alias, and Scala's `Either` type, construct a type called `IntOrString` that
   * can either hold an `Int` or a `String`.
   */
  type IntOrString = Either[Int, String]

  /**
   * EXERCISE 2
   *
   * Construct a value of type `IntOrString` that contains the string "Sherlock", using the
   * `Right(_)` constructor for `Either`.
   */
  lazy val intOrString: IntOrString = Left(42) //Right("Sherlock")

  /**
   * EXERCISE 3
   *
   * Using both a type alias, and Scala's `Either` type, construct a type called
   * `PaymentMethod` that can be either `CreditCard` or `WireTransfer`.
   */
  type PaymentMethod = Either[CreditCard, WireTransfer]

  final case class CreditCard()
  final case class WireTransfer()

  /**
   * EXERCISE 4
   *
   * Construct a value of type `PaymentMethod` that contains a value of type `CreditCard`, using the
   * `Left(_)` constructor for `Either`.
   */
  lazy val paymentMethod: PaymentMethod =
    Left(CreditCard())
}

/**
 * Scala supports case classes, which are a generic way to store two or more pieces of
 * information at the same time, where each piece of information can have a user-defined label
 * associated with it. Case classes are nicer than tuples because the elements of the tuple can
 * be accessed by identifiers, instead of by indices. Like tuples, case classes come with lots of
 * free functionality, including constructors, deconstructors, equality and hash code, string
 * representation for debugging, and a copy method.
 *
 * Case classes can be thought of as "records" that have zero or more "fields"; or they can be
 * thought of as defining "tables" that have zero or more "columns".
 *
 * Case classes are immutable: once constructed, they cannot change. However, there are simple ways
 * to created a new value from an existing value, in which some field has been "changed".
 *
 * Case classes are examples of "labeled products": they are types formed using "product
 * composition"  of other types, where each term of the product can be accessed by a user-defined
 * (unique) label.
 */
object enum_basics extends scala.App {

  /**
   * EXERCISE 1
   *
   * Using enums, construct a `ChessPieceType` type that can be one of the different types that
   * any chess piece can be, including a pawn, knight, bishop, rook, king, or queen.
   */
  sealed trait ChessPieceType
  object ChessPieceType {
    case object Pawn   extends ChessPieceType
    case object Knight extends ChessPieceType
    case object Bishop extends ChessPieceType
    case object Rook   extends ChessPieceType
    case object King   extends ChessPieceType
    case object Queen  extends ChessPieceType

    val pawn: ChessPieceType =
      Pawn
  }

  val pawn1: ChessPieceType.Pawn.type = ChessPieceType.Pawn
  val pawn2: ChessPieceType           = ChessPieceType.pawn

  pawn1 == pawn2 // true

  sealed trait Duration {
    def +(other: Duration): Duration =
      ???
  }
  case object Zero              extends Duration
  case class Finite(value: Int) extends Duration

  def zero: Duration =
    Zero

  val durations = List(Finite(1), Finite(2), Finite(3))

  durations.foldLeft(zero)(_ + _)

  // case class
  // case object - class with no parameters

  // final case class Person(name: String)
  final case class Queen()

  val queen1 = Queen()
  val queen2 = Queen()
  queen1 == queen2

  sealed trait ValidationResult[+A] {
    def computationName: String
    def resultOption: Option[A] =
      this match {
        case ValidationResult.Success(_, result) => Some(result)
        case ValidationResult.Failure(_, e)      => None
      }
  }

  object ValidationResult {
    case class Success[A](computationName: String, result: A) extends ValidationResult[A]
    case class Failure(computationName: String, e: Throwable) extends ValidationResult[Nothing]
  }

  sealed trait ProcessingResult

  object ProcessingResult {
    case class Success() extends ProcessingResult
    case class Failure() extends ProcessingResult
  }

  /**
   * EXERCISE 2
   *
   * Using the enum that you created, construct a value of type `ChessPieceType` that holds a
   * `Pawn`.
   */
  lazy val chessPieceType: ChessPieceType =
    ChessPieceType.pawn

  /**
   * EXERCISE 3
   *
   * Using enums, construct a `Currency` type, whose cases can hold currency-specific values, for
   * USD, EURO, and other currencies.
   */
  sealed trait Currency
  object Currency {
    final case class USD(dollars: Int, cents: Int) extends Currency
    final case class EUR(euros: Int, cents: Int)   extends Currency
    final case class MXN(pesos: Int, centavo: Int) extends Currency

    def usd(dollars: Int, cents: Int): Option[Currency] =
      if (dollars < 0 || cents < 0 || cents > 99)
        None
      else
        Some(USD(dollars, cents))
  }

  /**
   * EXERCISE 3
   *
   * Using the enum that you created, construct a value of type `Currency` that holds 9 dollars and
   * 9 cents of USD.
   */
  lazy val currency: Currency =
    Currency.USD(dollars = 9, cents = 9)
}

/**
 * Like case classes, Scala's enums come equipped with useful functionality that all "data
 * classes" should have. In particular, they have equality, hash; code, and string representation,
 * as well as support for pattern matching on individual cases of the enum.
 */
object enum_utilities {
  sealed trait Card
  object Card {
    final case class Clubs(points: Int)    extends Card
    final case class Diamonds(points: Int) extends Card
    final case class Spades(points: Int)   extends Card
    final case class Hearts(points: Int)   extends Card
  }

  Card.Clubs(10) match {
    case Card.Clubs(10) => println("Matched!")
    case _              => println("Did not match!")
  }

  lazy val card: Card = Card.Spades(10)

  /**
   * EXERCISE 1
   *
   * Pattern match on the value `card`. If the value is a `Spades`, then print out the number of
   * points of the card. Otherwise, ignore it. Note: You can match all values using the wildcard
   * (`_`), e.g. `case _ => `.
   */
  card match {
    case Card.Spades(points) => println(points)
    case _                   =>
  }

  /**
   * EXERCISE 2
   *
   * As with case classes, pattern matches on the cases of an enum may utilize literals. Pattern
   * match on `card` with a case that looks for `Spades(10)` (a spades card with 10 points), and
   * then have a catch all case. Print out different messages in each case.
   */
  card match {
    case Card.Spades(10) => println("Matched!")
    case _               => println("Did not match!")
  }

  /**
   * EXERCISE 3
   *
   * As with case classes, pattern matches on the cases of an enum may utilize conditionals. Pattern
   * match on `card` again, and have two cases: one that looks for `Spades` with points `>= 10`,
   * and a catch-all. Print out distinct messages in each case.
   */
  card match {
    case Card.Spades(p) if p >= 10 => println(p)
    case _                         => println("not spades with points >=10")
  }

  /**
   * EXERCISE 4
   *
   * As with pattern matching on case classes, when pattern matching on enumerations, any piece of
   * a pattern match may be captured and placed into a new variable by using the as-pattern syntax
   * `x @ ...`, where `x` is any legal variable name. The variable introduced by an as-pattern may
   * be used both in any conditional, or inside the case of the pattern match.
   *
   * In this exercise, match for `Spades`, give it a name `spades`, and print it out.
   */
  card match {
    case cards @ Card.Spades(points) => println(cards)
    case _                           =>
  }

  /**
   * EXERCISE 5
   *
   * As with case classes, the `|` pattern operator lets you match against two alternatives,
   * providing neither introduces new variables. In the context of enumerations, this operator
   * provides a nice way to look for one among a small number of different cases.
   *
   * In this exercise, match for either Spades or Diamonds, and print out a message.
   */
  card match {
    case Card.Spades(_) | Card.Diamonds(_) => println("got a spade or a diamond")
    case _                                 =>
  }
}

/**
 * Scala's enums can be generic, which means the enum may introduce a type parameter which can
 * be used in the cases of the enum. This allows building general-purpose and versatile data
 * structures that can have different types "plugged" into them in different contexts.
 */
object enum_generics {

  /**
   * EXERCISE 1
   *
   * Take the `AdvertisingEvent` enum, and make it generic, by introducing a new type parameter
   * called `Data`, and using `Data` for the type of the field `Data` that is stored in some of the
   * cases of the enum.
   */
  sealed trait AdvertisingEvent[+Data]
  object AdvertisingEvent {
    case object None                                                               extends AdvertisingEvent[Nothing]
    final case class Impression[Data](pageUrl: String, data: Data)                 extends AdvertisingEvent[Data]
    final case class Click[Data](pageUrl: String, elementId: String, data: Data)   extends AdvertisingEvent[Data]
    final case class Action[Data](pageUrl: String, actionName: String, data: Data) extends AdvertisingEvent[Data]
  }

  /**
   * EXERCISE 2
   *
   * Create a type alias called `AdvertisingEventString`, which plugs the type `String` into the
   * enum `AdvertisingEvent`, to create an advertising event where the data type is `String`.
   */
  type AdvertisingEventString = AdvertisingEvent[String]

  trait JSON

  val myAdvertisingEvent: AdvertisingEvent[JSON] =
    AdvertisingEvent.Impression("www.google.com", new JSON {})

  myAdvertisingEvent match {
    case AdvertisingEvent.None                              =>
    case AdvertisingEvent.Impression(pageUrl, data)         =>
    case AdvertisingEvent.Click(pageUrl, elementId, data)   =>
    case AdvertisingEvent.Action(pageUrl, actionName, data) =>
  }

  type AdvertisingEventBig = AdvertisingEvent[String]

  /**
   * EXERCISE 3
   *
   * Construct an `AdvertisingEvent` that stores data of type `Int`, namely, the integer `42`.
   */
  lazy val advertisingEvent: AdvertisingEvent[Int] =
    AdvertisingEvent.Impression("www.google.com", 42)

  /**
   * EXERCISE 4
   *
   * Take the `ConcatList` enum, and make it generic, by introducing a new type parameter called
   * `Element`, and using `Element` for the type of the field `value` that is stored in some of the
   * cases of the enum.
   */
  sealed trait ConcatList[+Element] {
    def concat[Element1 >: Element](that: ConcatList[Element1]): ConcatList[Element1] =
      ConcatList.Concat(this, that)
  }
  object ConcatList {
    case object Empty                                                                       extends ConcatList[Nothing]
    final case class Concat[Element](left: ConcatList[Element], right: ConcatList[Element]) extends ConcatList[Element]
    final case class One[Element](value: Element)                                           extends ConcatList[Element]
  }

  trait Animal
  trait Cat extends Animal
  trait Dog extends Animal

  val cats: ConcatList[Cat] = ???
  val dogs: ConcatList[Dog] = ???

  val animals = cats concat dogs

}
