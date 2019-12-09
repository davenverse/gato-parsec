package io.chrisdavenport.gatoparsec

import cats._
import cats.data._
import cats.implicits._
import io.chrisdavenport.gatoparsec.ParseResult.Done
import io.chrisdavenport.gatoparsec.ParseResult.Fail
import io.chrisdavenport.gatoparsec.ParseResult.Partial

sealed trait ParseResult[-Input, +Output]{
  
  def map[B](f: Output => B): ParseResult[Input, B] = this match {
    case Done(excess, result) =>  Done(excess, f(result))
    case Fail(input, stack, message) => Fail(input, stack, message)
    case Partial(k) => Partial(k.andThen(_ map f))
  }

  def feed(i: Input): ParseResult[Input, Output] = this match {
    case Done(excess, result) => Done(excess :+ i, result)
    case Partial(k) => k(Chain.one(i))
    case Fail(_, _, _) => this
  }

  def feedMany(i: Chain[Input]): ParseResult[Input, Output] = this match {
    case Done(excess, result) => Done(excess <+> i, result)
    case Partial(k) => k(i)
    case Fail(_, _, _) => this
  } 
}

object ParseResult {
  final case class Fail[Input, Output](input: Chain[Input], stack: List[String],  message: String)
    extends ParseResult[Input, Output]
  
  final case class Partial[Input, Output](k: Chain[Input] => ParseResult[Input, Output])
    extends ParseResult[Input, Output]

  final case class Done[Input, Output](excess: Chain[Input], result: Output)
    extends ParseResult[Input, Output]
  

  implicit def parseResultFunctor[Input] : Functor[ParseResult[Input, *]] = 
    new Functor[ParseResult[Input, *]]{
      def map[A, B](fa: ParseResult[Input,A])(f: A => B): ParseResult[Input,B] = 
        fa.map(f)
    }
}