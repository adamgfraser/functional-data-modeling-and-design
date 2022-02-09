package design

object Thunks extends App {

  final case class Lazy[+A](run: () => A)

  def delay[A](a: => A): Lazy[A] =
    Lazy(() => a)

  val helloWorld: Lazy[Unit] =
   delay(println("Hello, World!"))

  val printHelloWorldTwice = {
    helloWorld.run()
    helloWorld.run()
  }
}