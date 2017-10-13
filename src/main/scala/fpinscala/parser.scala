package fpinscala


trait Parsers[ParseError, Parser[+_]] {self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char]
  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] =
    ParserOps(f(a))

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
  implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)


  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

}

case class SimpleParser[+A](source: A)
case class SimpleParserException(msg: String) extends RuntimeException(msg)

object SimpleParsers extends Parsers[SimpleParserException, SimpleParser] {

  override def run[A](p: SimpleParser[A])(input: String): Either[SimpleParserException, A] = ???

  override implicit def char(c: Char): SimpleParser[Char] = SimpleParser(c)

  override implicit def string(s: String): SimpleParser[String] = SimpleParser(s)

  override def or[A](p1: SimpleParser[A], p2: SimpleParser[A]): SimpleParser[A] = ???

  override def listOfN[A](n: Int, p: SimpleParser[A]): SimpleParser[List[A]] = ???
}