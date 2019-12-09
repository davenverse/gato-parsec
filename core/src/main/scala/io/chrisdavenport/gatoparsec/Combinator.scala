package io.chrisdavenport.gatoparsec

import cats._
import cats.implicits._
import cats.data.Chain
import cats.data.NonEmptyList
import Parser._

object Combinator {
  /** Parser that consumes no data and produces the specified value. */
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def ok[Input, Output](a: Output): Parser[Input, Output] =
    new OkParser[Input, Output](a)

  private class OkParser[Input, Output](a: Output) extends  Parser[Input, Output]{
    override def toString(): String = s"ok(${a.toString()})"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Output) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      Eval.defer(ks(st0,a))
  }

  /** Parser that consumes no data and fails with the specified error message. */
  def err[Input, Output](what: String): Parser[Input, Output] = 
    new ErrParser[Input, Output](what)
  private class ErrParser[Input, Output](what: String) extends Parser[Input, Output]{
    override def toString(): String = s"err($what)"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Output) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      Eval.defer(kf(st0, Nil, what))
  }
  
  /** Construct the given parser lazily; useful when defining recursive parsers. */
  def delay[Input, Output](p: => Parser[Input, Output]): Parser[Input, Output] =
    new DelayParser[Input, Output](p)

  private class DelayParser[Input, Output](p: => Parser[Input,Output]) extends Parser[Input, Output]{
    override def toString(): String = p.toString
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Output) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      p.apply(st0, kf, ks)
  }

  
  /////

  def advance[Input](n: Int): Parser[Input, Unit] = 
    new AdvanceParser[Input, Unit](n)
  private class AdvanceParser[Input, Output](n: Int) extends Parser[Input, Unit]{
    override def toString(): String = s"advance($n)"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Unit) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      ks(st0.copy(pos = Parser.Pos(st0.pos.value + n)), ())
  }

  private def prompt[Input, Output](
    st0: State[Input],
    kf: State[Input] => Eval[Internal.Result[Input, Output]],
    ks: State[Input] => Eval[Internal.Result[Input, Output]]
  ): Parser.Internal.Result[Input, Output] =
    Parser.Internal.Partial[Input, Output](s =>
    if (s.isEmpty) Eval.defer(kf(st0.copy(complete =IsComplete.Complete)))
    else Eval.defer(ks(st0.copy(input = st0.input <+> s, complete = IsComplete.NotComplete)))
  )

  def demandInput[Input]: Parser[Input, Unit] = new DemandInputParser[Input]

  private class DemandInputParser[Input] extends Parser[Input, Unit]{
    override def toString = "demandInput"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Unit) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      if (st0.complete.bool) Eval.defer(kf(st0, List(), "not enough input"))
      else Eval.now(
        prompt(
          st0,
          st => kf(st, List(), "not enough input"),
          a => ks(a, ()))
      )
  }

  def ensureSuspended[Input](n: Int): Parser[Input, Chain[Input]] =
    new EnsureSuspendedParser[Input](n)
  private class EnsureSuspendedParser[Input](n: Int) extends Parser[Input, Chain[Input]]{
    override def toString(): String = s"ensureSuspended($n)"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], Chain[Input]) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = 
      if (st0.input.length >= st0.pos.value + n)
        // Can Do Better - Chain Methods Inadequate
        Eval.defer(ks(st0, Chain.fromSeq(st0.input.toList.drop(st0.pos.value).take(n))))
      else 
        Eval.defer(ensureSuspended(n)(st0, kf, ks))
  }

  def ensure[Input](n: Int): Parser[Input, Chain[Input]] =
    new EnsureParser[Input](n)
  private class EnsureParser[Input](n: Int) extends Parser[Input, Chain[Input]]{
    override def toString(): String = s"ensure($n)"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], Chain[Input]) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = 
      if (st0.input.length >= st0.pos.value + n)
        Eval.defer(ks(st0, Chain.fromSeq(st0.input.toList.drop(st0.pos.value).take(n))))
      else
        Eval.defer(ensureSuspended(n)(st0, kf, ks))
  }

  def wantInput[Input]: Parser[Input, Boolean] =
    new WantInputParser[Input] 
  private class WantInputParser[Input] extends  Parser[Input, Boolean]{
    override def toString = "wantInput"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], Boolean) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = 
      if (st0.input.length >= st0.pos.value + 1) Eval.defer(ks(st0, true))
      else if (st0.complete.bool) Eval.defer(ks(st0, false))
      else Eval.now(prompt(st0, a => ks(a, false), a => ks(a, true)))
  }

  /////

  /** Parser that produces the remaining input (but does not consume it). */
  def get[Input]: Parser[Input, Chain[Input]] = 
    new GetParser[Input]
  private class GetParser[Input] extends Parser[Input, Chain[Input]]{
    override def toString = "get"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Chain[Input]) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      Eval.defer(ks(st0, Parser.chainDrop(st0.input, st0.pos.value)))
  }

  /* Parser that produces the current offset in the input. */
  def pos[Input]: Parser[Input, Int] = 
    new PosParser[Input]
  private class PosParser[Input] extends Parser[Input, Int]{
    override def toString = "pos"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Int) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      Eval.defer(ks(st0, st0.pos.value))
  }
  
  def endOfChunk[Input]: Parser[Input, Boolean] =
    new EndOfChunkParser[Input]
  private class EndOfChunkParser[Input] extends Parser[Input, Boolean]{
    override def toString = "endOfChunk"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Boolean) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] =
      Eval.defer(ks(st0, st0.pos.value == st0.input.length))
  }

  def endOfInput[Input]: Parser[Input, Unit] =
    new EndOfInputParser[Input]
  private class EndOfInputParser[Input] extends Parser[Input, Unit]{
    override def toString = "endOfInput"
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Unit) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] = Eval.defer{
      if (st0.pos.value >= st0.input.length) {
        if (st0.complete.bool) ks(st0, ())
        else demandInput(
          st0,
          (st1: State[Input], _: List[String], _: String) => ks(st1, ()),
          (st1: State[Input], _: Unit) => kf(st1, Nil, "endOfInput")
        )
      } else {
        kf(st0, Nil, "endOfInput")
      }
    }
  }

  
  def discardLeft[Input, A, B](m: Parser[Input, A], b: => Parser[Input, B]): Parser[Input, B] = 
    new DiscardLeftParser[Input,A, B](m, b)
  private class DiscardLeftParser[Input, A, B](
    m: Parser[Input, A],
    b:  => Parser[Input, B]
  ) extends Parser[Input, B]{
    override def toString(): String = s"($m) ~> $b"
    def apply[R](
        st0: State[Input], 
        kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
        ks: (State[Input], B) => Eval[Internal.Result[Input, R]]
      ): Eval[Internal.Result[Input, R]] = {
        Eval.defer(
          m(st0, kf, (s: State[Input], _ : A) => b(s, kf, ks))
        )
      }
  }

  def discardRight[Input, A, B](m: Parser[Input, A], b: Parser[Input, B]): Parser[Input, A] = 
    new DiscardRightParser(m, b)
  private class DiscardRightParser[Input, A, B](m: Parser[Input, A], b: Parser[Input, B]) extends Parser[Input, A]{
    override def toString(): String = s"($m) <~ $b"
    def apply[R](
        st0: State[Input], 
        kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
        ks: (State[Input], A) => Eval[Internal.Result[Input, R]]
      ): Eval[Internal.Result[Input, R]] = {
        Eval.defer(
          m(
            st0,
            kf, 
            (st1: State[Input], a: A) => 
              b(st1, kf, (st2: State[Input], _: B) => ks(st2, a)
            )
          )
        )
      }
  }


  def andThen[Input, A, B](m: Parser[Input, A], b: => Parser[Input, B]): Parser[Input, (A, B)] =
    new AndThenParser[Input, A, B](m, b)
  private class AndThenParser[Input, A, B](m: Parser[Input, A], b: => Parser[Input, B]) extends Parser[Input, (A, B)]{
    override def toString(): String = s"($m) ~ $b"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], (A, B)) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = Eval.defer(
      m(st0, kf, (st1: State[Input], a: A) => b(st1, kf, (st2: State[Input], b: B) => ks(st2, (a, b))))
    )
  }


  def orElse[Input, A, B >: A](m: Parser[Input, A], b: => Parser[Input, B]): Parser[Input, B] = 
    new OrElseParser(m, b)
  private class OrElseParser[Input, A, B >: A](m: Parser[Input, A], b: => Parser[Input, B]) extends Parser[Input, B]{
    override def toString(): String = s"($m) | $b"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], B) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = Eval.defer(
      m(st0, (st1: State[Input], _: List[String], _: String) => b(st1.copy(pos = st0.pos), kf, ks), ks)
    )
  }


  def either[Input, A, B](m: Parser[Input, A], b: => Parser[Input, B]): Parser[Input, Either[A, B]] = 
    new EitherParser[Input, A, B](m, b)
  private class EitherParser[Input, A, B](m: Parser[Input, A], b: => Parser[Input, B]) extends Parser[Input, Either[A, B]]{
    override def toString(): String = s"($m) || $b"
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], Either[A, B]) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = Eval.defer(m(
      st0,
      (st1: State[Input], _: List[String], _: String) => 
        b(st1.copy(pos= st0.pos), kf, (st1: State[Input], b: B) => ks(st1, Right(b))),
      (st1: State[Input], a: A) => ks(st1, Left(a))
    ))
  }


  def named[Input, A](m: Parser[Input, A], s: => String): Parser[Input, A] = 
    new NamedParser(m, s)
  private class NamedParser[Input, A](m: Parser[Input, A], s: => String) extends Parser[Input, A]{
    override def toString = s
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], A) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = 
      Eval.defer(m(st0, (st1: State[Input], stack: List[String], msg: String) => kf(st1, s:: stack, msg), ks))
  }

  def namedOpaque[Input, A](m: Parser[Input, A], s: => String): Parser[Input, A] =
    new NamedOpaqueParser(m, s)
  private class NamedOpaqueParser[Input, A](m: Parser[Input, A], s: => String) extends Parser[Input, A]{
    override def toString(): String = s
    def apply[R](
      st0: Parser.State[Input],
      kf: (Parser.State[Input], List[String], String) => Eval[Parser.Internal.Result[Input,R]],
      ks: (Parser.State[Input], A) => Eval[Parser.Internal.Result[Input,R]]
    ): Eval[Parser.Internal.Result[Input,R]] = Eval.defer(
      m(st0, (st1: State[Input], _: List[String], _: String) => kf(st1, Nil, "Failure reading:" + s), ks)
    )
  }


  def modifyName[Input, A](m: Parser[Input, A], f: String => String): Parser[Input, A] = 
    named(m, f(m.toString()))

  // Higher Level Combinators
  // Allowed to Use Implicits Based on the above
  import io.chrisdavenport.gatoparsec.implicits._

  def filter[Input, A](m: Parser[Input, A])(p: A => Boolean): Parser[Input, A] = 
    m.flatMap{a => 
      if (p(a)) ok[Input, A](a) else err[Input, A]("filter")
    } named "filter(...)"
  
  def collect[Input, A, B](m: Parser[Input, A], f: PartialFunction[A, B]): Parser[Input, B] = 
    filter(m)(f isDefinedAt _).map(f)

  def cons[Input, A, B >: A](m: Parser[Input, A], n: => Parser[Input, List[B]]): Parser[Input, NonEmptyList[B]] = 
    m.flatMap{x => n.map(xs => NonEmptyList(x, xs))}

  def phrase[Input, A](p: Parser[Input, A]): Parser[Input, A] = 
    p <~ endOfInput named ("phrase" + p.toString())

  def many[Input, A](p: => Parser[Input, A]): Parser[Input, List[A]] = {
    lazy val many_p : Parser[Input, List[A]] = cons(p, many_p).map(_.toList) | ok(Nil)
    many_p named "many(" + p.toString() + ")"
  }

  def many1[Input, A](p: Parser[Input, A]): Parser[Input, NonEmptyList[A]] =
    cons(p, many(p))

  def manyN[Input, A](n: Int, a: Parser[Input, A]): Parser[Input, List[A]] = 
    (1.to(n)).foldRight(ok[Input, List[A]](List[A]()))((_, p) => cons(a, p).map(_.toList))
      .named("ManyN(" + n.toString + ", " + a.toString + ")")

  def manyUntil[Input, A](p: Parser[Input, A], q: Parser[Input, _]): Parser[Input, List[A]] = {
    lazy val scan : Parser[Input, List[A]] = q ~> ok(Nil) | cons(p, scan).map(_.toList)
    scan named "manyUntil(" + p.toString() + "," + q.toString() + ")"
  }

  def skipMany[Input](p: Parser[Input, _]): Parser[Input, Unit] = 
    many(p).map(_ => ()) named s"skipMany($p)"
  
  def skipMany1[Input](p: Parser[Input, _]): Parser[Input, Unit] = 
    many1(p).map(_ => ()) named s"skipMany1($p)"
  
  def skipManyN[Input](n: Int, p: Parser[Input, _]): Parser[Input, Unit] =
    manyN(n, p).map(_ => ()) named s"skipManyN($n,$p)"

  def sepBy1[Input, A](p: Parser[Input, A], s: Parser[Input, _]): Parser[Input, NonEmptyList[A]] = {
    lazy val scan : Parser[Input, NonEmptyList[A]] = 
      cons(p, s ~> scan.map(_.toList) | ok(Nil))
    
    scan named s"sepBy1($p,$s)"
  }

  def sepBy[Input, A](p: Parser[Input, A], s: Parser[Input, _]): Parser[Input, List[A]] = {
    cons(p, ((s ~> sepBy1(p,s)).map(_.toList) | ok(List.empty[A]))).map(_.toList) | ok(List.empty[A]) named ("sepBy(" + p.toString + "," + s.toString + ")")
  }

  def pairBy[Input, A, B](a: Parser[Input, A], delim: Parser[Input, _], b: Parser[Input, B]): Parser[Input, (A, B)] = 
    (a <~ delim) ~ b

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def choice[Input, A](xs: Parser[Input, A]*) : Parser[Input, A] =
    xs.foldRight[Parser[Input, A]](err("choice: no match"))(_ | _) named s"choice(${xs.mkString(", ")})"

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def choice[F[_]: Foldable, Input, A](fpa: F[Parser[Input, A]]): Parser[Input, A] =
    choice(fpa.toList: _*)

  def opt[Input, A](m: Parser[Input, A]): Parser[Input, Option[A]] =
    (m.map[Option[A]](Some(_)) | ok(Option.empty[A])) named s"opt($m)"

  def count[Input, A](n: Int, p: Parser[Input, A]): Parser[Input, List[A]] =
    (1 to n).foldRight(ok[Input, List[A]](List[A]()))((_, a) => cons(p, a).map(_.toList))
  
}