package design

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */


/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter =
      !(!self && !that)

    def ^^(that: EmailFilter): EmailFilter =
      (self || that) && !(self && that)
    
    def unary_! : EmailFilter = EmailFilter.Not(self)
  }
  object EmailFilter {
    final case object Always                                            extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter)         extends EmailFilter
    final case class Not(filter: EmailFilter)                           extends EmailFilter
    final case class SenderIn(targets: Set[Address])                    extends EmailFilter
    final case class RecipientIn(targets: Set[Address])                 extends EmailFilter
    final case class BodyContains(phrase: String)                       extends EmailFilter
    final case class SubjectContains(phrase: String)                    extends EmailFilter

    val always: EmailFilter = Always

    // always && x === x
    // never || x === x

    val never: EmailFilter = !always

    def senderIs(sender: Address): EmailFilter = senderIn(Set(sender))

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIs(recipient: Address): EmailFilter = recipientIn(Set(recipient))

    def recipientIsNot(recipient: Address): EmailFilter = !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components extends App {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  // trait Turtle { self =>
  //   def turnLeft(degrees: Int): Unit

  //   def turnRight(degrees: Int): Unit

  //   def goForward(): Unit

  //   def goBackward(): Unit

  //   def draw(): Unit
  // }

  sealed trait Turtle { self =>

    def >>>(that: Turtle): Turtle =
      andThen(that)

    def andThen(that: Turtle): Turtle =
      Turtle.AndThen(self, that)
  }

  object Turtle {
    case object DoNothing extends Turtle
    final case class AndThen(first: Turtle, second: Turtle) extends Turtle
    final case class Turn(degrees: Int) extends Turtle
    final case object Go extends Turtle

    val go: Turtle =
      Go

    val doNothing: Turtle =
      DoNothing

    val goBackward: Turtle =
      turnAround andThen go

    lazy val turnAround: Turtle =
      turnRight(180)

    def turnLeft(degrees: Int): Turtle =
      turnRight(360 - degrees)

    def turnRight(degrees: Int): Turtle =
      Turn(degrees)
  }

  val turtles: List[Turtle] = ???

  turtles.foldLeft(Turtle.doNothing)(_ >>> _)

  // if (turtles.isEmpty) println("No turtles")
  // else turtles.reduce(_ >>> _).draw()

  val turnRight = Turtle.Turn(90)
  val go = Turtle.Go

  val myTurtle: Turtle = (turnRight >>> go >>> (turnRight >>> go)) >>> turnRight

  // go >>> turnLeft
  // turnLeft >>> go

  final case class State(x: Int, y: Int, theta: Int)

  object State {
    val origin: State = State(0, 0, 0)
  }

  final case class Canvas(values: Vector[Vector[Boolean]])

  def draw(turtle: Turtle): Unit = {

    def loop(turtle: Turtle, state: State): State = {
      turtle match {
        case Turtle.AndThen(first, second) =>
          val updatedState = loop(first, state)
          loop(second, updatedState)
        case Turtle.Turn(degrees) =>
          println(s"The turtle turned $degrees degrees")
          state.copy(theta = (state.theta + degrees) % 360)
        case Turtle.Go =>
          println("The turtle moved forward one pixel")
          state
        case Turtle.DoNothing =>
          state
      }
    }

    loop(turtle, State.origin)
  }

  println(myTurtle)
  draw(myTurtle)

  // AndThen(Turn(90),Go)

  // coordinates
  // direction

  // draw

  // 1. Data type that represents the solution to some problem
  // Constructors
  // Operators
    // Binary operators - (data types, data type) => data type
    // composability
    // Emailfilter combine emailfilter ==> emailfilter

}
