package io.chrisdavenport.gatoparsec
package bench

import implicits._
import Combinator._

import cats.data.Chain
import cats.implicits._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class ParserBench {
  val a = elem[Char].filter(_ === 'a')
  val b = elem[Char].filter(_ === 'b')
  val aOrB: Parser[Char, Char] = a | b

  @Benchmark
  def streamingSmallReadSmallChunks(): Unit = {
    val data = (0 until 10000).map(_ => Chain('a', 'b'))
    val res = data.foldLeft(Parser.parse(many(aOrB), Chain.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res.feedMany(Chain.empty) match {
      case ParseResult.Done(_, _) => true
      case _ => false
    }
    assert(isDone)
  }

  @Benchmark
  def streamingBigReadSmallChunksCount(): Unit = {
    val bigNum = 10000
    val data = (0 until bigNum).map(_ => Chain('a', 'b'))
    val p = count(bigNum * 2 - 1, elem[Char].void) ~> elem[Char]
    val res = data.foldLeft(Parser.parse(p, Chain.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res match {
      case ParseResult.Done(_, 'b') => true
      case _ => false
    }
    assert(isDone)
  }

  @Benchmark
  def streamingBigReadSmallChunksTake(): Unit = {
    val bigNum = 10000
    val data = (0 until bigNum).map(_ => Chain('a', 'b'))
    val p = take[Char](bigNum * 2 - 1).void ~> elem[Char]
    val res = data.foldLeft(Parser.parse(p, Chain.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res match {
      case ParseResult.Done(_, 'b') => true
      case _ => false
    }
    assert(isDone)
  }
}
