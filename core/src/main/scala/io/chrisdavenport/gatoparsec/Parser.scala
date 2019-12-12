package io.chrisdavenport.gatoparsec

import cats._
import cats.data._

trait Parser[Input, Output]{
  import Parser._
  def apply[R](
    st0: State[Input], 
    kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
    ks: (State[Input], Output) => Eval[Internal.Result[Input, R]]
  ): Eval[Internal.Result[Input, R]]

  def flatMap[B](f: Output => Parser[Input, B]): Parser[Input, B] = 
    new ParserFlatMap[Input, Output, B](this, f)
  def map[B](f: Output => B): Parser[Input, B] = 
    new ParserMap(this, f)

  override def toString(): String = "Parser(...)"
}

object Parser {

  def parse[Input, Output](p: Parser[Input, Output], input: Chain[Input]): ParseResult[Input, Output] = {
    def kf(a: State[Input], b: List[String], c: String) = Eval.now[Internal.Result[Input, Output]](
      Internal.Fail(a.copy(input = a.cursor), b, c)
    )
    def ks(a: State[Input], b: Output) = Eval.now[Internal.Result[Input, Output]](
      Internal.Done(
        a.copy(input = a.cursor),
        b
      )
    )

    p(State.apply(input, IsComplete.NotComplete), kf, ks).value.translate
  }

  def parseOnly[Input, Output](p: Parser[Input, Output], input: Chain[Input]): ParseResult[Input, Output] = {
    def kf(a: State[Input], b: List[String], c: String) = Eval.now[Internal.Result[Input, Output]](
      Internal.Fail(a.copy(input = a.cursor), b, c)
    )
    def ks(a: State[Input], b: Output) = Eval.now[Internal.Result[Input, Output]](
      Internal.Done(
        a.copy(input = a.cursor),
        b
      )
    )
    p(State.apply(input, IsComplete.Complete), kf, ks).value.translate
  }

  sealed trait IsComplete {
    def bool: Boolean = this match {
      case IsComplete.Complete => true
      case IsComplete.NotComplete => false
    }
  }
  object IsComplete {
    case object Complete extends IsComplete
    case object NotComplete extends IsComplete
  }
  final case class State[Input](input: Chain[Input], cursor: Chain[Input], complete: IsComplete)
  object State {
    def apply[Input](input: Chain[Input], done: IsComplete): State[Input] = 
      new State(input, input, done)
  }
  
  object Internal {
    sealed trait Result[Input, Output]{
      def translate: ParseResult[Input, Output] = this match {
        case Done(input, result) => ParseResult.Done(input.input, result)
        case Fail(input, stack, message) => ParseResult.Fail(input.input, stack, message)
        case Partial(k) => ParseResult.Partial(a => k(a).value.translate)
      }
    }
    final case class Fail[Input, Output](input: State[Input], stack: List[String], message: String)
      extends Result[Input, Output]
    final case class Partial[Input, Output](k: Chain[Input] => Eval[Result[Input, Output]])
      extends Result[Input, Output]
    final case class Done[Input, Output](input: State[Input], result: Output)
      extends Result[Input, Output]
  }

  implicit def ParserMonad[I]: Alternative[Parser[I, *]] with Monad[Parser[I, *]] = 
    new Alternative[Parser[I, *]] with StackSafeMonad[Parser[I, *]]{
      def pure[A](x: A): Parser[I,A] = Combinator.ok[I](x)
      def flatMap[A, B](fa: Parser[I,A])(f: A => Parser[I,B]): Parser[I,B] = fa.flatMap(f)
      def combineK[A](x: Parser[I,A], y: Parser[I,A]): Parser[I,A] = Combinator.orElse(x, y)
      def empty[A]: Parser[I,A] = Combinator.err[I, A]("zero")
    }

  private[gatoparsec] def chainDrop[A](c: Chain[A], i: Int):  Chain[A] = Chain.fromSeq(
    c.iterator.drop(i).toSeq
  )

  private class ParserFlatMap[Input, Output1, Output2](
    p: Parser[Input, Output1],
    f: Output1 => Parser[Input, Output2]
  ) extends Parser[Input, Output2]{
    override def toString(): String = s"($p) flatMap ..."
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Output2) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] = 
      Eval.defer(p( st0, kf, (s: State[Input], a: Output1) => f(a)(s,kf,ks)))
  }

  private class ParserMap[Input, Output1, Output2](
    p: Parser[Input, Output1],
    f: Output1 => Output2
  )extends Parser[Input, Output2]{
    override def toString(): String = s"($p) map ..."
    def apply[R](
      st0: State[Input], 
      kf: (State[Input], List[String], String) => Eval[Internal.Result[Input, R]],
      ks: (State[Input], Output2) => Eval[Internal.Result[Input, R]]
    ): Eval[Internal.Result[Input, R]] = 
    Eval.defer(p(st0,kf,(s: State[Input], a: Output1) => Eval.defer(ks(s,f(a)))))
  }
}
