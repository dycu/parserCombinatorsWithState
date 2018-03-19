import cats.{Applicative, Functor, Monad}

object ParserCombinatorsWithState {

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

//  def string(s: String): Parser[String] = new Parser[String] {
//    def run = input => input.map(char).toList.sequence
//  }

  def some[A](p: Parser[A]): Parser[List[A]] = ???

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  object ParserInstances {
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

      override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
    }
  }

  def main(args: Array[String]) = {
    import ParserInstances._
    import cats.implicits._

    val f = (x: Char) => x.toUpper

    val p = Applicative[Parser].pure(f).ap(char('a'))

    val r = for {
      _ <- char('a')
      b <- char('b')
    } yield b

    println(
      r.run("abaaaa")
    )
  }
}
