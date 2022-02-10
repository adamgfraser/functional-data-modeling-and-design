package design

import scala.annotation.tailrec

object WhileLoops extends App {

  def imperative(as: Array[Int]): Int = {
    var sum = 0
    var i = 0
    while (i < as.length) {
      sum += as(i)
      i += 1
    }
    sum
  }

  def functional(as: Array[Int]): Int = {

    @tailrec
    def loop(sum: Int, i: Int): Int =
      if (i < as.length) loop(sum + as(i), i + 1)
      else sum

    loop(0, 0)
  }

  val result1 = imperative(Array(1, 2, 3, 4, 5))
  println(result1)

  val result2 = functional(Array(1, 2, 3, 4, 5))
  println(result2)
}