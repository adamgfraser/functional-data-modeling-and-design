package fdm
import scala.util.Failure
import scala.util.Success

/**
 * There are several types in Scala that have special meaning.
 */
object special_types {

  // read-permission     write-permission

  // You have neither
  // You have both
  // You can read but not write
  // There is no such thing as write but not read

  final case class Permission(canRead: Boolean, canWrite: Boolean)

  val myPermission: Permission = ???

  myPermission match {
    case Permission(true, true)   => println("You have read write")
    case Permission(true, false)  => println("You can read only")
    case Permission(false, true)  => ??? // impossible
    case Permission(false, false) => println("Get out of our system")
  }

  // Types can have sizes
  // Cardinality
  // How many different values can a type have?

  // Nothing - 0
  // Unit    - 1
  // Boolean - 2 - true, false

  // sum types
  // product types

  // Either[Boolean, Boolean] - four possible values
  // Left(false), Left(true) ,Right(false), Right(true)

  // Either[Unit, Boolean] - three possible values
  val first: Either[Unit, Boolean]  = Left(())
  val second: Either[Unit, Boolean] = Right(true)
  val third: Either[Unit, Boolean]  = Right(false)

  // Either[A, B]
  // A has cardinary X
  // B has cardinality of Y
  // What's the cardinary of Either[A, B]?
  // A + B

  // (Boolean, Boolean)
  // its a product type!
  // A * B

  // A + 1
  // Either[Unit, A] <-> Option[A]

  // A + 1
  // sealed trait Option[A]

  // object Option {
  //   case object Empty extends Option[A] // Like () only has one possible value
  //   case class Some[A](value: A) extends Option[A] // A possible values
  // }

  // Option is like an either when the error contains no useful information

  final case class Equivalence[A, B](to: A => B, from: B => A)

  object Equivalence {

    def optionEitherUnitEquivalence[A]: Equivalence[Option[A], Either[Unit, A]] =
      Equivalence(
        option =>
          option match {
            case None        => Left(())
            case Some(value) => Right(value)
          },
        either =>
          either match {
            case Left(_)      => None
            case Right(value) => Some(value)
          }
      )
  }

  // type MyComplexType = Either[Option[A], Either[B, C]]

  // Left(None)
  // Left(A)
  // Right(Left(B))
  // Right(Right(C))

  sealed trait MyComplexType

  object MyComplexType {
    case object ThereWasNoValue extends MyComplexType
    final case class GotAnA[A](a: A) extends MyComplexType
    final case class GotaB[B](b: B) extends MyComplexType
    final case class GotaC[C](c: C) extends MyComplexType
  }

  /**
   * EXERCISE 1
   *
   * Find a type existing in the Scala standard library, which we will call `One`, which has a
   * single "inhabitant" (i.e. there exists a single unique value that has this type).
   */
  type One = Unit

  /**
   * EXERCISE 2
   *
   * Find a type existing in the Scala standard library, which we will call `Zero`, which has no
   * "inhabitants" (i.e. there exists no values of this type).
   */
  type Zero = Nothing

  /**
   * EXERCISE 3
   *
   * Scala allows you to treat `Nothing` as any other type. To demonstrate this to yourself,
   * change the return type of this function to whatever type you like, then try to explain why
   * this rule in the Scala compiler will not lead to any crashes of your application.
   */
  def nothingIsAnything(value: Nothing): Nothing = value
}

/**
 * Data types that are formed from case classes (products) and enums (sums) are sometimes called
 * _algebraic data types_. The word _algebraic_ here refers to the _algebraic laws_ that are
 * satisfied by type-level operators that compose types.
 */
object algebra {

  // (1, 2) = 1 * 2 = 2
  // (2, 1) = 2 * 1 = 2

  /**
   * EXERCISE 3
   *
   * If we use `*` to denote product composition of types, then as with ordinary multiplication on
   * numbers, we should have that `A * B` is the same as `B * A`.
   *
   * Although the tuples (A, B) and (B, A) are not exactly the same, they are equivalent because
   * they store the same amount of information. For example, (Boolean, String) stores the same
   * amount of information as (String, Boolean). In math and in functional programming, this
   * equivalence is called an "isomorphism", and it can be regarded as a weaker but more useful
   * definition of equality.
   */
  def toBA[A, B](ab: (A, B)): (B, A) =
    ab match {
      case (a, b) => (b, a)
    }
  def toAB[A, B](ba: (B, A)): (A, B) =
    ba match {
      case (b, a) => (a, b)
    }

  def roundtripAB[A, B](t: (A, B)): (A, B) = toAB(toBA(t))
  def roundtripBA[A, B](t: (B, A)): (B, A) = toBA(toAB(t))

  /**
   * EXERCISE 4
   *
   * If we use `+` to sum product composition of types, then as with ordinary addition on
   * numbers, we should have that `A + B` is the same as `B + A`.
   *
   * Although the eithers Either[A, B] and Either[B, A] are not exactly the s;ame, they are
   * isomorphic, as with tuples.
   */
  def toBA[A, B](ab: Either[A, B]): Either[B, A] =
    ab match {
      case Left(a)  => Right(a)
      case Right(b) => Left(b)
    }
  def toAB[A, B](ba: Either[B, A]): Either[A, B] =
    ba match {
      case Left(b)  => Right(b)
      case Right(a) => Left(a)
    }

  def roundtripAB[A, B](t: Either[A, B]): Either[A, B] = toAB(toBA(t))
  def roundtripBA[A, B](t: Either[B, A]): Either[B, A] = toBA(toAB(t))

  /**
   * EXERCISE 5
   *
   * As with multiplication of numbers, we also have `A * 1` is the same as `A`.
   */
  def withUnit[A](v: A): (A, Unit)    = (v, ())
  def withoutUnit[A](v: (A, Unit)): A = v._1

  def roundtripUnit1[A](v: A): A                 = withoutUnit(withUnit(v))
  def roundtripUnit2[A](t: (A, Unit)): (A, Unit) = withUnit(withoutUnit(t))

  /**
   * EXERCISE 6
   *
   * As with multiplication of numbers, we also have `A + 0` is the same as `A`.
   */
  def withNothing[A](v: A): Either[A, Nothing]    =
    Left(v)
  def withoutNothing[A](v: Either[A, Nothing]): A =
    v match {
      case Left(a)  => a
      case Right(e) => e
    }

    // final case Right(a: A) extends Either

  def roundtripNothing1[A](v: A): A                                   = withoutNothing(withNothing(v))
  def roundtripNothing2[A](t: Either[A, Nothing]): Either[A, Nothing] = withNothing(withoutNothing(t))

  /**
   * EXERCISE 7
   *
   * As with multiplication of numbers, we also have `A * 0` is the same as `0`.
   */
  def withValue[A](v: Nothing): (A, Nothing)    = (v, v)
  def withoutValue[A](v: (A, Nothing)): Nothing = v._2

  def roundtripValue1(v: Nothing): Nothing              = withoutValue(withValue(v))
  def roundtripValue2[A](t: (A, Nothing)): (A, Nothing) = withValue(withoutValue(t))

  /**
   * EXERCISE 8
   *
   * Algebraic data types follow the distributive property, such that `A * (B + C) = A * B + A * C`.
   */
  def distribute[A, B, C](tuple: (A, Either[B, C])): Either[(A, B), (A, C)] = TODO
  def factor[A, B, C](either: Either[(A, B), (A, C)]): (A, Either[B, C])    =
    either match {
      case Left((a, b))  => (a, Left(b))
      case Right((a, c)) => (a, Right(c))
    }

  def roundtripDist1[A, B, C](t: (A, Either[B, C])): (A, Either[B, C])           = factor(distribute(t))
  def roundtripDist2[A, B, C](e: Either[(A, B), (A, C)]): Either[(A, B), (A, C)] = distribute(factor(e))
}

// sealed trait Employee

// object Employee {

//   final case class Manager(name: String)
//   final case class Engineer(name: String)
// }

final case class Employee(name: String, role: Role)

sealed trait Role

case object Role {
  case object Manager extends Role
  case object Engineer extends Role
}

/**
 * The algebraic notation introduced previously is quite flexible and can model all data types,
 * including those that are generic and recursive.
 */
object algebra_of_types {
  final case class Identity[A](value: A)

  /**
   * EXERCISE 1
   *
   * All functional data types can be written using sums and products, and therefore have an
   * algebraic definition. For polymorphic data types, the algebraic definition may refer to the
   * polymorphic types. For example, the algebraic definition of the identity type shown above is
   * `A`.
   *
   * Create a polymorphic data type whose algebraic definition is `A * B`. Hint: You can use
   * `Tuple2` or create your own version of this data type.
   */
  type ATimesB

  /**
   * EXERCISE 2
   *
   * Create a polymorphic data type whose algebraic definition is `A + B`. Hint: You can use
   * `Either` or create your own version of this data type.
   */
  type APlusB

  /**
   * EXERCISE 3
   *
   * The algebraic definition of recursive polymorphic data types can be expressed using infinite
   * polynomial series. For example, the type of `List[A]` can be defined as
   * `1 + A + A * A + A * A * A + ...`, or, by using exponentiation, `1 + A + A^2 + A^3 + ...`.
   *
   * Find the algebraic definition for the following type `Tree`.
   */
  sealed trait Tree[+A]
  object Tree {
    final case class Leaf[+A](value: A)                      extends Tree[A]
    final case class Fork[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
  }
}

/**
 * Different types can be equivalent, and looking at their algebraic definitions can make this
 * equivalence easier to see.
 */
object algebraic_equivalence {

  /**
   * EXERCISE 1
   *
   * The type `Either[String, Option[Int]]` can be difficult to work with, if we are concerned
   * about manipulating the `Int`. Fortunately, this type is equivalent to another one, which is
   * easier to work with. Find some type `Answer1` such that `Either[Answer1, Int]` is equivalent
   * to the first type.
   *
   * Write out the algebraic definitions of both types, and show they are equivalent.
   */
  type ComplexEither = Either[String, Option[Int]]
  // type Answer1 = Option[String]
  type SimplerEither = Either[Option[String], Int]

  // type Pull[E, A] = Either[Option[E], A]

  // Three possibilities:
  // 1. Stream is done with an error
  // 2. Stream is done with end of stream signal
  // 3. Stream has a value right now

  sealed trait Pull[+E, +A] 

  // How many possibilities?
  // E + A + 1
  object Pull {
    case object Done extends Pull[Nothing, Nothing]
    case class Error[E](e: E) extends Pull[E, Nothing]
    case class Next[A](a: A) extends Pull[Nothing, A]
  }

  /**
   * EXERCISE 2
   *
   * The type `Option[A]` is actually unnecessary, because it is equivalent to `Either[Answer2, A]`,
   * for some type `Answer2`. Find what this type is and write your answer below.
   *
   * Write out the algebraic definitions of both `Option` and your new type, and show they are
   * equivalent.
   */
  type Answer2
  type NewOption[+A] = Either[Unit, A]

  /**
   * EXERCISE 3
   *
   * The type `Try[A]` is also unnecessary. Find a type `Answer3` such that `Either[Answer3, A]` is
   * equivalent to `Try[A]`.
   *
   * Write out the algebraic definitions of both `Try` and your new type, and show they are
   * equivalent.
   */
  type Answer3
  type NewTry[+A] = Either[Throwable, A]

  import scala.util.Try

  // sealed trait Try[+A]


  // // A + T
  // object Try {
  //   case class Success[+A](value: A) extends Try[A]
  //   case class Failure(t: Throwable) extends Try[Nothing]
  // }

  final case class Equivalence[A, B](to: A => B, from: B => A)

  object Equivalence {

    def tryEitherThrowableEquivalence[A]: Equivalence[Try[A], Either[Throwable, A]] =
      Equivalence(
        scalaTry =>
          scalaTry match {
            case Failure(exception) => Left(exception)
            case Success(value) => Right(value)
          },
        either =>
          either match {
            case Left(exception) => Failure(exception)
            case Right(value) => Success(value)
          }
      )
  }
}
