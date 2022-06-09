package example

object TF {
  // Defining a basic CPS-like "Algebra",
  // this is basically an AST; it defines the DSL semantics
  // through types. For example, halt is the only operation
  // which returns an Int, so we use it last unless we are
  // running a program for side effects.
  trait Algebra[F[_]] {
    var env: Map[String, Int] = Map()

    def lit(n: String, x: Int): F[Unit]
    def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String): F[Unit]
    def print(n: String): F[Unit]
    def halt(n: String): F[Int]

    // contrived derived method
    def print_and_halt(n: String) = {
      print(n)
      halt(n)
    }
  }

  // Defining an interpreter for the Algebra, we could
  // easily write a different one with e.g. a different
  // implementation of print which works for testing.
  case class Expr[A](res: A)
  class CPSInterpreter extends Algebra[Expr] {
    override def lit(n: String, x: Int): Expr[Unit] = {
      env = env + (n -> x)
      Expr(())
    }

    override def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String): Expr[Unit] = {
      env = env + (n -> prim(env(lhs), env(rhs)))
      Expr(())
    }

    override def print(n: String): Expr[Unit] = {
      println(env(n))
      Expr(())
    }

    override def halt(n: String): Expr[Int] = {
      Expr(env(n))
    }
  }

  // This interpreter, instead of printing to stdout,
  // adds it to a log, which can be better for testing
  class TestCPSInterpreter extends CPSInterpreter {
    var log: Vector[String] = Vector()

    override def print(n: String): Expr[Unit] = {
      log = log :+ s"${n}: ${env(n)}"
      Expr(())
    }
  }

  // Finally, we define a program that runs the interpreter.
  // Ideally we would add an effect constraint to F, but the
  // flexibility of tagless final is that we can easily choose
  // what effect/handler to use.
  
  def prog1[F[_]]: Algebra[F] ?=> F[Int] = (interpreter: Algebra[F]) ?=> {

    // c5 <- 5
    // res <- c5 * c5
    // res2 <- res * c5
    // print(res2)
    // return res
    interpreter.lit("c5", 5)
    interpreter.prim("res", (a, b) => a * b, "c5", "c5")
    interpreter.prim("res2", (a, b) => a * b, "res", "c5")
    interpreter.print("res2")
    interpreter.print_and_halt("res")
  }

  def res1: Int = {
    given Algebra[Expr] = new CPSInterpreter
    val Expr(res) = prog1[Expr]
    res
  }

  def res2: (Int, Vector[String]) = {
    given interpreter: TestCPSInterpreter = new TestCPSInterpreter
    val Expr(res) = prog1[Expr]
    (res, interpreter.log)
  }
}
