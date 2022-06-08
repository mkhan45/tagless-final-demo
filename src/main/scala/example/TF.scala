package example

object TF {
  trait Algebra[E[_]] {
    def start(): E[Unit]
    def lit(n: String, x: Int)(state: E[Unit]): E[Unit]
    def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String)(state: E[Unit]): E[Unit]
    def print(n: String)(state: E[Unit]): E[Unit]
    def halt(n: String)(state: E[Unit]): E[Int]
  }

  case class Expr[A](map: Map[String, Int], ret: A)
  implicit object CPSInterpreter extends Algebra[Expr] {
    override def start(): Expr[Unit] = {
      Expr(Map(), ())
    }

    override def lit(n: String, x: Int)(state: Expr[Unit]): Expr[Unit] = {
      Expr(state.map + (n -> x), ())
    }

    override def prim(n: String, prim: (Int, Int) => Int, lhs: String, rhs: String)(state: Expr[Unit]): Expr[Unit] = {
      Expr(state.map + (n -> prim(state.map(lhs), state.map(rhs))), ())
    }

    override def print(n: String)(state: Expr[Unit]): Expr[Unit] = {
      println(state.map(n))
      Expr(state.map, ())
    }

    override def halt(n: String)(state: Expr[Unit]): Expr[Int] = {
      Expr(state.map, state.map(n))
    }
  }

  def prog1[F[_]](implicit interpreter: Algebra[F]): F[Int] = {
    import scala.util.chaining._

    interpreter.start()
      .pipe(interpreter.lit("c5", 5))
      .pipe(interpreter.prim("res", (a, b) => a * b, "c5", "c5"))
      .pipe(interpreter.prim("res2", (a, b) => a * b, "res", "c5"))
      .pipe(interpreter.print("res2"))
      .pipe(interpreter.halt("res"))
  }

  def res1: Int = {
    val Expr(_, res) = prog1[Expr]
    res
  }
}
