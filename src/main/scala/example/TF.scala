package example

object TF {
  // Defining a basic CPS-like "Algebra",
  // this is basically an AST; it defines the DSL semantics
  // through types. For example, halt is the only operation
  // which returns an Int, so we use it last unless we are
  // running a program for side effects.
  trait Algebra[F[_]] {
    var map: Map[String, Int] = Map()

    def lit(n: String, x: Int): F[Unit]
    def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String): F[Unit]
    def print(n: String): F[Unit]
    def halt(n: String): F[Int]
  }

  // Defining an interpreter for the Algebra, we could
  // easily write a different one with e.g. a different
  // implementation of print which works for testing.
  case class Expr[A](res: A)
  class CPSInterpreter extends Algebra[Expr] {
    override def lit(n: String, x: Int): Expr[Unit] = {
      map = map + (n -> x)
      Expr(())
    }

    override def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String): Expr[Unit] = {
      map = map + (n -> prim(map(lhs), map(rhs)))
      Expr(())
    }

    override def print(n: String): Expr[Unit] = {
      println(map(n))
      Expr(())
    }

    override def halt(n: String): Expr[Int] = {
      Expr(map(n))
    }
  }

  // This interpreter, instead of printing to stdout,
  // adds it to a log, which can be better for testing
  class TestCPSInterpreter extends CPSInterpreter {
    var log: Vector[String] = Vector()

    override def print(n: String): Expr[Unit] = {
      log = log :+ s"${n}: ${map(n)}"
      Expr(())
    }
  }

  // Finally, we define a program that runs the interpreter.
  // Ideally we would add an effect constraint to F, but the
  // flexibility of tagless final is that we can easily choose
  // what effect/handler to use.
  def prog1[F[_]](implicit interpreter: Algebra[F]): F[Int] = {
    import scala.util.chaining._

    interpreter.lit("c5", 5)
    interpreter.prim("res", (a, b) => a * b, "c5", "c5")
    interpreter.prim("res2", (a, b) => a * b, "res", "c5")
    interpreter.print("res2")
    interpreter.halt("res")
  }

  def res1: Int = {
    val Expr(res) = prog1[Expr](new CPSInterpreter)
    res
  }

  def res2: (Int, Vector[String]) = {
    val it = new TestCPSInterpreter
    val Expr(res) = prog1[Expr](it)
    (res, it.log)
  }
}
