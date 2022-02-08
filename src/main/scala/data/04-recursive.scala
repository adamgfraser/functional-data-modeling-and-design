package fdm

/**
 * Scala data types constructed from enums and case classes may be *recursive*: that is, a top-
 * level definition may contain references to values of the same type.
 */
object recursive {

  /**
   * EXERCISE 1
   *
   * Create a recursive data type that models a user of a social network, who has friends; and
   * whose friends may have other friends, and so forth.
   */
  final case class User(id: Long, friends: Set[User])

  /**
   * EXERCISE 2
   *
   * Create a recursive data type that models numeric operations on integers, including addition,
   * multiplication, and subtraction.
   */
  sealed trait NumericExpression
  object NumericExpression {
    final case class Literal(value: Int) extends NumericExpression
    final case class Addition(left: NumericExpression, right: NumericExpression) extends NumericExpression
    final case class Multiplication(left: NumericExpression, right: NumericExpression) extends NumericExpression
    final case class Subtraction(left: NumericExpression, right: NumericExpression) extends NumericExpression
  }

  /**
   * EXERCISE 3
   *
   * Create a `EmailTrigger` recursive data type which models the conditions in which to trigger
   * sending an email. Include common triggers like on purchase, on shopping cart abandonment, etc.
   */
  sealed trait EmailTrigger
  object EmailTrigger {
    case object OnPurchase                                         extends EmailTrigger
    case object OnShoppingCartAbandonment                         extends EmailTrigger
    final case class CustomerAgeGreaterThan(days: Int) extends EmailTrigger
    final case class Both(left: EmailTrigger, right: EmailTrigger) extends EmailTrigger // send an email if left and right both want to send e-mail
    final case class Either(left: EmailTrigger, right: EmailTrigger) extends EmailTrigger // send an email if either left or right want to send email
  }
}

/**
 * As Scala is an eager programming language, in which expressions are evaluated eagerly, generally
 * from left to right, top to bottom, the tree-like data structures created with case classes and
 * enumerations do not contain cycles. However, with some work, you can model cycles. Cycles are
 * usually for fully general-purpose graphs.
 */
object cyclically_recursive extends App {
  final case class Snake(food: Snake)

  /**
   * EXERCISE 1
   *
   * Create a snake that is eating its own tail. In order to do this, you will have to use a
   * `lazy val`.
   */
  lazy val snake: Snake =
    Snake(snake)

  /**
   * EXERCISE 2
   *
   * Create two employees "Tim" and "Tom" who are each other's coworkers. You will have to change
   * the `coworker` field from `Employee` to `() => Employee` (`Function0`), also called a "thunk",
   * and you will have to use a `lazy val` to define the employees.
   */
  final case class Employee(name: String, coworker: () => Employee)

  lazy val tim: Employee = Employee("Tim", () => tom)
  lazy val tom: Employee = Employee("Tom", () => tim)

  println(tim.name)
  println(tim.coworker().name)

  /**
   * EXERCISE 3
   *
   * Develop a List-like recursive structure that is sufficiently lazy, it can be appended to
   * itself.
   */
  sealed trait LazyList[+A] extends Iterable[A] { self =>
    def iterator: Iterator[A] =
      new Iterator[A] {
        var lazyList = self
        def hasNext: Boolean =
          lazyList match {
            case LazyList.Empty => false
            case LazyList.Cons(head0, tail0) => true
          }
        def next(): A =
          lazyList match {
            case LazyList.Empty => throw new NoSuchElementException
            case LazyList.Cons(head0, tail0) =>
              lazyList = tail0()
              head0()
          }
      }
  }

  object LazyList {

    case object Empty extends LazyList[Nothing] 
    final case class Cons[+A](head0: () => A, tail0: () => LazyList[A]) extends LazyList[A]

    def apply[A](el: A): LazyList[A] =
      Cons(() => el, () => Empty)

    // The syntax `=>` means a "lazy parameter". Such parameters are evaluated wherever they are
    // referenced "by name".
    def concat[A](left: => LazyList[A], right: => LazyList[A]): LazyList[A] =
      left match {
        case Empty => right
        case Cons(h, t) => Cons(h, () => concat(t(), right))
      }
  }

  lazy val infiniteList: LazyList[Int] = LazyList.concat(LazyList(1), infiniteList)

  println(infiniteList.take(10))

  // Avoid cyclically recursive structures if possible
  // if you have a domain that does have this cyclical structure you can model it in Scala
  // if you need to use cyclical structures use these tools:
    // thunks () => A
    // lazy val
    // may need to specify more type parameters
}
