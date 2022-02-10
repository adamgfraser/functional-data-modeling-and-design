package design
import design.recipes.Baked.Burnt
import design.recipes.Baked.Undercooked

/*
 * INTRODUCTION
 *
 * In Functional Design, operators that transform and compose values in a
 * domain often fall into pre-existing patterns.
 *
 * In this section, you'll learn to identify these patterns.
 *
 */

// binary operators

// && / intersection
// || / union
// andThen / +, ++
// orElse

// unary operators

// negation

// associativity
// (x + y) + z === x + (y + z)

// (5 - 1) - 2 === ???
// 5 - (1 - 2) === 6

// commutativity

// (x + y) === (y + x)

// myFilter1 = filter1 && filter2
// myFilter2 = filter2 && filter1

/**
 * BINARY COMPOSITION PATTERNS FOR VALUES - EXERCISE SET 1
 */
object binary_values {
  object Exercise1 {

    /**
     * EXERCISE 1
     *
     * Choose a type such that you can implement the `compose` function in
     * such a way that:
     *
     * {{{
     * compose(compose(a, b), c) == compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`.
     */
    type SomeType = Int

    def compose(left: SomeType, right: SomeType): SomeType =
      left + right
  }

  object Exercise2 {

    /**
     * EXERCISE 2
     *
     * Choose a different type such that you can implement the `compose`
     * function in such a way that:
     *
     * {{{
     * compose(compose(a, b), c) == compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`.
     */
    type SomeType = Boolean

    // (a && b) && c === a && (b && c)
    // "Adam" + ("Grant" + "Fraser") === ("Adam" + "Grant") + "Fraser"

    // Boolean conjunction
    // Boolean disjunction
    // String concatenation
    // Integer addition
    // Integer multiplication
    // List concatenation
    // Istream concatenation
    // Turtle andThen
    // Union or intersection on the email filters and the schedules

    // Subtraction is not associative
    // Set difference is not associative

    final case class StatsCounter(sum: Double, count: Int = 1) { self =>
      def <>(that: StatsCounter): StatsCounter =
        StatsCounter(self.sum + that.sum, self.count + that.count)
      def mean: Double =
        sum / count
    }

    val statsCounter1 = StatsCounter(18.246)
    val statsCounter2 = StatsCounter(21.456)
    val statsCounter3 = StatsCounter(5.343)
    val statsCounter4 = (statsCounter1 <> statsCounter2) <> statsCounter3
    val statsCounter5 = statsCounter1 <> (statsCounter2 <> statsCounter3)
    println(statsCounter4 == statsCounter5)

    def compose(left: SomeType, right: SomeType): SomeType =
      left || right
  }

  object Exercise3 {

    /**
     * EXERCISE 3
     *
     * Choose a type such that you can implement the `compose`
     * function in such a way that:
     *
     * {{{
     * compose(a, b) == compose(b, a)
     * }}}
     *
     * for all `a`, `b`.
     */
    type SomeType = Int

    def compose(left: SomeType, right: SomeType): SomeType =
      left + right
  }

  object Exercise4 {

    /**
     * EXERCISE 4
     *
     * Choose a different type such that you can implement the `compose`
     * function in such a way that:
     *
     * {{{
     * compose(a, b) == compose(b, a)
     * }}}
     *
     * for all `a`, `b`.
     */
    type SomeType

    List(1, 2, 3) ++ List(4, 5, 6)
    List(4, 5, 6) ++ List(1, 2, 3)

    // test("integer addition is associative") {
    //   check(Gen.anyInt, Gen.anyInt, Gen.anyInt) { (x, y, z) =>
    //     val left = (x + y) + z
    //     val right = x + (y + z)
    //     assertTrue(left == right)
    //   }
    // }

    // implicit val intAdditionAssociative: Associative[Int] =
    //   new Associative[Int] {
    //     def combine(left: Int, right: Int): Int =
    //       left + right
    //   }

    // Associative.laws.checkAllLaws(intAdditionAssociative)(Gen.anyInt)

    // a <> b === b <> a

    val capitals = Map("UK" -> "London", "France" -> "Paris")
    val food     = Map("UK" -> "Curry", "France"  -> "Wine")

    val map1 = capitals ++ food
    val map2 = food ++ capitals

    // def combineByKeyIntoKeySet[K, V](left: Map[K, Set[V]], right: Map[K, Set[V]]): Map[K, Set[V]] =
    //   ???

    // Things that satisfy commutative property
    // Boolean conjunction and disjunction
    // integer addition and multiplication
    // Set union and intersection

    // Things that don't satisfy commutative property
    // List concatenation

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise5 {

    // and + or
    // product type + sum type
    // case class + sealed trait
    // "both" + "either"

    // "And"
    // Email filter &&
    // Schedule intersection
    // Istream ++
    // List concatenation
    // Turtle andThen

    /**
     * EXERCISE 5
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "both". For example, if you have
     * a data type that represents a query, then this `compose` could
     * combine two queries into one query, such that both results would
     * be queried when the model is executed.
     */
    type SomeType = design.input_stream.IStream

    def compose(left: SomeType, right: SomeType): SomeType =
      left ++ right
  }

  object Exercise6 {

    /**
     * EXERCISE 6
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "both".
     */
    type SomeType = design.pricing_fetcher.Schedule

    def compose(left: SomeType, right: SomeType): SomeType =
      left && right
  }

  object Exercise7 {

    // Boolean disjunction
    // IStream orElse
    // Listener orElse
    // Either orElse
    // Try orElse
    // Option orElse
    // ZIO orElse

    /**
     * EXERCISE 7
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "or". For example, if you have
     * a data type that represents a query, then this `compose` could
     * model running one query, but if it fails, running another.
     */
    type SomeType = design.input_stream.IStream

    def compose(left: SomeType, right: SomeType): SomeType =
      left orElse right
  }

  object Exercise8 {

    /**
     * EXERCISE 8
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "or".
     */
    type SomeType = Either[String, Int]

    def compose(left: SomeType, right: SomeType): SomeType =
      left.orElse(right)
  }

  // inverse

  // val emailFilter = ???
  // val emailFilter2 = emailFilter.negate.negate

  // idempotency
  // val startingValue = ???
  // val value1 = startingValue <> a
  // val value2 = startingValue <> a <> a
  // value1 === value2

  // Set union
  // Set(1) union Set(2) union Set(2)
  // Set(1) union Set(2)

  // Integer addition
  // x + 0 === x
  // 0 is an identity element with respect to integer addition

  // Integer multiplication
  /// x * ??? === x
  //  1 is an identity element with respect to multiplication

  // List concatenation
  /// List(1, 2, 3) ::: xs === List(1, 2, 3)
  // Nil aka List.empty is the identity element for list concatenation

  def sum(as: List[Int]): Int =
    as.foldLeft(0)(_ + _)

  object Exercise9 {

    /**
     * EXERCISE 9
     *
     * Choose a type and a value called `identity` such that you can implement
     * the `compose` function in such a way that:
     *
     * {{{
     * compose(a, identity) == compose(identity, a) == a
     * }}}
     *
     * for all `a`.
     */
    type SomeType = Boolean

    // b || ??? === b
    // true || false === true
    // false || false === false

    def identity: SomeType = true

    def compose(left: SomeType, right: SomeType): SomeType =
      left && right

    // associative and has identity element
    // associative and commutative but doesn't have an identity
    // associative and commutative and does have an identity
  }

  object Exercise10 {

    // empty IStream
    // Email always and never
    // intersection and combining of emails

    /**
     * EXERCISE 10
     *
     * Choose a different type and a value called `identity` such that you can
     * implement the `compose` function in such a way that:
     *
     * {{{
     * compose(a, identity) == compose(identity, a) == a
     * }}}
     *
     * for all `a`.
     */
    type SomeType

    def identity: SomeType = ???

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }
}

object TypeConstructors {

  type ??? = Nothing

  // Option[Int]

  // parametric polymorphism

  def f(x: Int): Int =
    ??? // infinite number

  def identity[A](a: A): A =
    a

  // sum and product types

  val myOption1  = Option(1)
  val myOption2  = Option(2)
  val optionUnit = Option(())
  val result     = myOption1.zip(myOption2)
  val result2    = myOption1.orElse(myOption2)

  val result3: Option[Int] = myOption1.zip(optionUnit).map(_._1)
  // result3 === myOption1

  // (option1 orElse option2) orElse option3
  // option1 orElse (option2 orElse option3)

  // option1.zip(option2).zip(option3)

  // ((A, B), C))
  // (A, (B, C))

  def any: Option[Any] =
    Option(())

  def nothing: Option[Nothing] =
    None

  // any.zip(option) === option
  // nothing.orElse(option) === option

  def zip[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
    (left, right) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None
    }

  def zipEither[E, A, B](left: Either[E, A], right: Either[E, B]): Either[E, (A, B)] =
    (left, right) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(e), _)         => Left(e)
      case (_, Left(e))         => Left(e)
    }

  // None is an identity element with respect orElseEither for an Option

  // Left biased either
  def orElseEither[A, B](left: Option[A], right: Option[B]): Option[Either[A, B]] =
    left match {
      case Some(a) => Some(Left(a))
      case None =>
        right match {
          case Some(b) => Some(Right(b))
          case None    => None
        }
    }

  def orElse[A](left: Option[A], right: Option[A]): Option[A] =
    orElseEither(left, right).map(_.merge)

// result1 orElse result2

  // // Right biased either
  // def either2[A, B](left: Option[A], right: Option[B]): Option[Either[A, B]] =
  //   right match {
  //     case Some(b) => Some(Right(b))
  //     case None =>
  //       left match {
  //         case Some(a) => Some(Left(a))
  //         case None    => None
  //       }
  //   }
}

/**
 * BINARY COMPOSITION PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 2
 */
object binary_tcs {
  object Exercise1 {

    /**
     * EXERCISE 1
     *
     * Choose a type such that you can implement the `compose` function in
     * such a way that:
     *
     * {{{
     * compose(compose(a, b), c) ~ compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`, where `~` means "equivalent to".
     */
    type SomeType[A] = Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      left.zip(right)
  }

  object Exercise2 {

    import zio._

    /**
     * EXERCISE 2
     *
     * Choose a different type such that you can implement the `compose` function
     * in such a way that:
     *
     * {{{
     * compose(compose(a, b), c) ~ compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`, where `~` means "equivalent to".
     */
    type SomeType[A] = ZIO[Any, Nothing, A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      left.zip(right)
  }

  object Exercise3 {

    /**
     * EXERCISE 3
     *
     * Choose a type such that you can implement the `compose` function in
     * such a way that:
     *
     * {{{
     * compose(compose(a, b), c) ~ compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`, where `~` means "equivalent to".
     */
    type SomeType[A] = Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      TypeConstructors.orElseEither(left, right)
  }

  object Exercise4 {
    import zio._

    /**
     * EXERCISE 4
     *
     * Choose a different type such that you can implement the `compose` function
     * in such a way that:
     *
     * {{{
     * compose(compose(a, b), c) ~ compose(a, compose(b, c))
     * }}}
     *
     * for all `a`, `b`, `c`, where `~` means "equivalent to".
     */
    type SomeType[A] = ZIO[Any, Throwable, A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.orElseEither(right)
  }

  object Exercise5 {

    /**
     * EXERCISE 5
     *
     * Choose a type such that you can implement the `compose` function in
     * such a way that:
     *
     * {{{
     * compose(a, b) ~ compose(b, a)
     * }}}
     *
     * for all `a`, `b`, where `~` means "equivalent to".
     */
    type SomeType[A] = Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise6 {
    import zio._

    /**
     * EXERCISE 6
     *
     * Choose a different type such that you can implement the `compose` function
     * in such a way that:
     *
     * {{{
     * compose(a, b) ~ compose(b, a)
     * }}}
     *
     * for all `a`, `b`, where `~` means "equivalent to".
     */
    type SomeType[A] = ZIO[Any, Throwable, A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      left.zipPar(right)
  }

  object Exercise7 {
    import zio._

    /**
     * EXERCISE 7
     *
     * Choose a type such that you can implement the `compose` function in
     * such a way that:
     *
     * {{{
     * compose(a, b) ~ compose(b, a)
     * }}}
     *
     * for all `a`, `b`, where `~` means "equivalent to".
     */
    type SomeType[A] = ZIO[Any, Throwable, A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.raceEither(right)
  }

  object Exercise8 {

    /**
     * EXERCISE 8
     *
     * Choose a different type such that you can implement the `compose` function
     * in such a way that:
     *
     * {{{
     * compose(a, b) ~ compose(b, a)
     * }}}
     *
     * for all `a`, `b`, where `~` means "equivalent to".
     */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise9 {

    /**
     * EXERCISE 9
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "both". For example, if you have
     * a data type that represents a query, then this `compose` could
     * combine two queries into one query, such that both results would
     * be queried when the model is executed.
     */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise10 {

    /**
     * EXERCISE 10
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "both".
     */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise11 {

    /**
     * EXERCISE 11
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "or". For example, if you have
     * a data type that represents a query, then this `compose` could
     * model running one query, but if it fails, running another.
     */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise12 {

    /**
     * EXERCISE 12
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "or".
     */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise13 {

    /**
     * EXERCISE 13
     *
     * Choose or create a type `SomeType` and a value called `identity` such
     * that you can implement the `compose` function in such a way that:
     *
     * {{{
     * compose(a, identity) ~ compose(identity, a) ~ a
     * }}}
     *
     * for all `a`, where `~` means "equivalent to".
     */
    type SomeType[A]

    def identity: SomeType[Any] = ???

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise14 {

    /**
     * EXERCISE 14
     *
     * Choose or create a type `SomeType` and a value called `identity` such
     * that you can implement the `compose` function in such a way that:
     *
     * {{{
     * compose(a, identity) ~ compose(identity, a) ~ a
     * }}}
     *
     * for all `a`, where `~` means "equivalent to".
     *
     * Note that `Either[A, Nothing]` is equivalent to `A`, and
     * `Either[Nothing, A]` is equivalent to `A`.
     */
    type SomeType[A]

    def identity: SomeType[Nothing] = ???

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

}

/**
 * IMPERATIVE PATTERNS FOR VALUES - EXERCISE SET 3
 */
object imperative_values {
  trait Exercise1 {

    /**
     * EXERCISE 1
     *
     * Choose or create a data type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType

    def andThen(first: SomeType, second: SomeType): SomeType = ???
  }

  trait Exercise2 {

    /**
     * EXERCISE 2
     *
     * Choose or create a different type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType

    def andThen(first: SomeType, second: SomeType): SomeType
  }
}

/**
 * IMPERATIVE PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 4
 */
object imperative_tcs {
  trait Exercise1 {
    import zio._

    // trait DataSource
    // object DataSource {
    //   case object InMemory
    //   case object OnDisk
    // }

    // def zio1: ZIO[Any, Nothing, DataSource] = ???
    // zio1.flatMap { dataSource =>
    //   if (datasource == DataSource.InMemory)
    //     ??? // doSomething
    //   else
    //     ??? // doSomethingElse
    //   ???
    // }

    // // Go forward
    // // Turn left

    /**
     * EXERCISE 1
     *
     * Choose or create a data type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType[A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B] = ???
  }

  trait Exercise2 {

    /**
     * EXERCISE 2
     *
     * Choose or create a different type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType[A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B]
  }
}

/**
 * RECIPES - GRADUATION PROJECT
 */
object recipes extends App {
  sealed trait Baked[+A]
  object Baked {
    final case class Burnt[A](value: A)         extends Baked[A]
    final case class CookedPerfect[A](value: A) extends Baked[A]
    final case class Undercooked[A](value: A)   extends Baked[A]
  }

  sealed trait Ingredient
  object Ingredient {
    final case class Eggs(number: Int)        extends Ingredient
    final case class Sugar(amount: Double)    extends Ingredient
    final case class Flour(amount: Double)    extends Ingredient
    final case class Cinnamon(amount: Double) extends Ingredient
  }

  // 1. Data type that is a solution to a problem in some domain
  // 2. Constructors for that data type
  // 3. Operators for combining (unary and binary)

  // A is the "output" of the recipe
  // What the recipe "produces"

  sealed trait Recipe[+A] { self =>

    /**
     * EXERCISE 1
     *
     * Implement a `map` operation that allows changing what a recipe produces.
     */
    def map[B](f: A => B): Recipe[B] =
      Recipe.Map(self, f)

    /**
     * EXERCISE 2
     *
     * Implement a `combine` operation that allows combining two recipes into
     * one, producing both items in a tuple.
     */
    def combine[B](that: Recipe[B]): Recipe[(A, B)] =
      for {
        a <- self
        b <- that
      } yield (a, b)

    def combineWith[B, C](that: Recipe[B])(f: (A, B) => C): Recipe[C] =
      self.combine(that).map(f.tupled)

    /**
     * EXERCISE 3
     *
     * Implement a `tryOrElse` operation that allows trying a backup recipe,
     * in case this recipe ends in disaster.
     */
    def tryOrElse[B](that: Recipe[B]): Recipe[Either[A, B]] =
      Recipe.TryOrElse(self, that)

    def orElse[A1 >: A](that: Recipe[A1]): Recipe[A1] =
      self.tryOrElse(that).map(_.merge)

    /**
     * EXERCISE 4
     *
     * Implement a `flatMap` operation that allows deciding which recipe to
     * make after this recipe has produced its item.
     *
     * NOTE: Be sure to update the `make` method below so that you can make
     * recipes that use your new operation.
     */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] =
      Recipe.FlatMap(self, f)

    def bake(temp: Int, time: Int): Recipe[Baked[A]] = Recipe.Bake(self, temp, time)
  }
  object Recipe {
    case object Disaster                                                   extends Recipe[Nothing]
    final case class AddIngredient(ingredient: Ingredient)                 extends Recipe[Ingredient]
    final case class Bake[A](recipe: Recipe[A], temp: Int, time: Int)      extends Recipe[Baked[A]]
    final case class Map[A, B](self: Recipe[A], f: A => B)                 extends Recipe[B]
    final case class TryOrElse[A, B](recipe: Recipe[A], backup: Recipe[B]) extends Recipe[Either[A, B]]
    final case class FlatMap[A, B](recipe: Recipe[A], f: A => Recipe[B])   extends Recipe[B]

    def addIngredient(ingredient: Ingredient): Recipe[Ingredient] = AddIngredient(ingredient)

    def disaster: Recipe[Nothing] = Disaster
  }
  import Recipe._

  def make[A](recipe: Recipe[A]): A =
    recipe match {
      case Disaster                  => throw new Exception("Uh no, utter disaster!")
      case AddIngredient(ingredient) => println(s"Adding ${ingredient}"); ingredient
      case Bake(recipe, temp, time) =>
        val a = make(recipe)

        println(s"Baking ${a} for ${time} minutes at ${temp} temperature")

        if (time * temp < 1000) Baked.Undercooked(a)
        else if (time * temp > 6000) Baked.Burnt(a)
        else Baked.CookedPerfect(a)
      case Map(recipe, f) =>
        val a = make(recipe)
        f(a)
      case TryOrElse(recipe, backup) =>
        try {
          Left(make(recipe))
        } catch {
          case _: Exception => Right(make(backup))
        }
      case FlatMap(recipe, f) =>
        val a = make(recipe)
        val b = f(a)
        make(b)
    }

  final case class Cake(ingredients: List[Ingredient])

  /**
   * EXERCISE 5
   *
   * Make a recipe that will produced a baked cake or other food of your choice!
   */
  lazy val recipe: Recipe[Baked[Cake]] = {
    val rawCake = for {
      eggs     <- addIngredient(Ingredient.Eggs(12))
      sugar    <- addIngredient(Ingredient.Sugar(1.5))
      flour    <- addIngredient(Ingredient.Flour(1.5))
      cinnamon <- addIngredient(Ingredient.Cinnamon(0.5))
      cake     = Cake(List(eggs, sugar, flour, cinnamon))
    } yield cake
    rawCake.bake(400, 120)
  }

  def tasteTestRecipe[A](recipe: Recipe[Baked[A]]): Recipe[Baked.CookedPerfect[A]] =
    recipe.flatMap {
      case Baked.CookedPerfect(a) =>
        println("It's perfect!")
        recipe.map(_ => Baked.CookedPerfect(a))
      case Baked.Burnt(_) =>
        println("You overcooked it!")
        disaster
      case Baked.Undercooked(value) =>
        println("You undercooked it!")
        disaster
    }

  val storeBoughtCake = {
    val rawCake = for {
      eggs     <- addIngredient(Ingredient.Eggs(6))
      sugar    <- addIngredient(Ingredient.Sugar(1))
      flour    <- addIngredient(Ingredient.Flour(1))
      cinnamon <- addIngredient(Ingredient.Cinnamon(1))
      cake     = Cake(List(eggs, sugar, flour, cinnamon))
    } yield cake
    rawCake.bake(400, 10)
  }

  val tasteTestedCake = make(tasteTestRecipe(recipe).orElse(storeBoughtCake))
  println(tasteTestedCake)

  // Executable - Schedule(lambda/?????)
  // Declarative - Schedule(very detailed text)

  // Bake(FlatMap(AddIngredient(Eggs(12)),design.recipes$$$Lambda$7571/0x0000000801e7f040@a513807),400,10)

}
