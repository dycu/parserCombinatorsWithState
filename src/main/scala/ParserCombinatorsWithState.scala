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

  trait Parser[A] {
    def run: String => Option[(A, String)]
  }

  def char(c: Char) = new Parser[Char] {
    def run = input => input.headOption.filter(_ == c).map((_, input.tail))
  }

  def main(args: Array[String]) = {
    println(
      char('a').run("aaaaa")
    )
  }
}
