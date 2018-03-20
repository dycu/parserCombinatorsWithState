import cats.{Applicative, Functor, Monad}
import cats.implicits._

object ParserCombinatorsWithState {
  implicit val parserFunctor = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = new Parser[B] {
      override def run: String => Option[(B, String)] = input =>
        fa.run(input).map { case (a, tail) => (f(a), tail) }
    }
  }

  trait ParserApplicative extends Applicative[Parser] {
    override def pure[A](x: A): Parser[A] = new Parser[A] {
      override def run: String => Option[(A, String)] = input => Some((x, input))
    }

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = new Parser[B] {
      override def run: String => Option[(B, String)] = input =>
        for {
          (a, r1) <- fa.run(input)
          (fab, r2) <- ff.run(r1)
        } yield (fab(a), r2)
    }
  }

  implicit val parserApplicative = new ParserApplicative {}

  implicit val parserMonad = new Monad[Parser] with ParserApplicative {
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = new Parser[B] {
      override def run: String => Option[(B, String)] = input =>
        for {
          (a, r1) <- fa.run(input)
          (b, r2) <- f(a).run(r1)
        } yield (b, r2)
    }

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
      def go(input: String, a: A): Option[(B, String)] = {
        f(a).run(input) match {
          case None => None
          case Some((Left(next), r)) => go(r, next)
          case Some((Right(result), r)) => Some((result, r))
        }
      }

      new Parser[B] {
        override def run: String => Option[(B, String)] = go(_, a)
      }
    }
  }

  /** Aim is to be able to parse the following:
    * var x = 1;
    * var y = x * 3;
    * x + y;
    * (x + 1) * 3 + 1;
    * var z = 8;
    *
    * with the following result: (Map(x -> 1, y -> 3, z -> 8), List(4, 7))
    */

  /** Step 1: Parsing of simple arithmetic language:
    * 1;
    * 2+2;
    * 3*3+4*4*(1+2);
    */

  /** TODO: change into: ?
    *
    * sealed trait ParseResult[+T]
    * trait Parser[+T] extends (Input => ParseResult[T])
    */
  trait Parser[A] {
    def run: String => Option[(A, String)]
  }

  def char(c: Char) = conditional(_ == c)

  def conditional(p: Char => Boolean) = new Parser[Char] {
    def run = input => input.headOption.filter(p).map((_, input.tail))
  }

  def string(s: String): Parser[String] = {
    s.map(char).toList.sequence[Parser, Char].map(_.mkString)
  }

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = new Parser[A] {
    override def run: String => Option[(A, String)] = input => p1.run(input).orElse(p2.run(input))
  }

  def many[A](p: Parser[A]): Parser[List[A]] = Monad[Parser].tailRecM(List[A]())(elems =>
    new Parser[Either[List[A], List[A]]] {
      override def run: String => Option[(Either[List[A], List[A]], String)] = input =>
        p.run(input) match {
          case None => Some((Right(elems.reverse), input))
          case Some((a, r)) => Some(Left(a :: elems), r)
        }
    }
  )

  def some[A](p: Parser[A]): Parser[List[A]] = (p, many(p)).mapN(_ :: _)

  def main(args: Array[String]) = {

//    val f = (x: Char) => x.toUpper
//
//    val p = Applicative[Parser].pure(f).ap(char('a'))
//
//    val r = for {
//      _ <- char('a')
//      b <- char('b')
//    } yield b

    val r = some(char('a'))

    println(
      r.run("acbaaaa")
    )
  }
}
