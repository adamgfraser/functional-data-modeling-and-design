package design

object VariableArguments {

  val myList = List(1, 2, 3, 4, 5)
  val myList2 = List(1, 2, 3)
  val myList3 = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  def sum(prefix: String, ints: Int*): Unit =
    println(prefix + " " + ints.sum)

  sum("a small summary", 1)
  sum("a slightly larger summary", 1, 2)
  sum("summary", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  def sum2(ints: Iterable[Int]): Int =
    ints.sum

  sum2(List(1, 2, 3, 4, 5))
}