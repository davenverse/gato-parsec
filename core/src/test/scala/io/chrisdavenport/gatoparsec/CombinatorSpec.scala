package io.chrisdavenport.gatoparsec

import io.chrisdavenport.gatoparsec.implicits._
import org.specs2._
import org.specs2.mutable.Specification
import scala.collection.immutable.Queue
import cats.implicits._

class CombinatorSpec extends Specification with ScalaCheck {
  "Combinator" should {
    "parse ok correctly always" >> prop { (s: String, n: Int) =>
      Combinator.ok[String](n).parseOnly(Queue(s)) match {
        case ParseResult.Done(_, _) => ok
        case _ => ko
      }
    }
    "err should always fail" >> prop {s: String => 
      Combinator.err[String, String]("oops").parseOnly(Queue(s)) match {
        case ParseResult.Fail(_, _, _) => ok
        case _ => ko
      }
    }

    "get should always succeed" >> prop { s: String => 
      Combinator.get[String].parseOnly(Queue(s)) match {
        case ParseResult.Done(_, _) => ok
        case _ => ko
      }
    }

    "pos should return correct position" >> prop {l: List[String] => 
      (l.length > 0) ==> {
        val simpleParser = for {
          _ <- Combinator.take[String](l.length - 1)
          p <- Combinator.pos
        } yield p

        simpleParser.parseOnly(l) match {
          case ParseResult.Done(_, position) => position must_=== (l.length - 1)
          case _ => ko
        }
      }
    }

    "advance should succeed" >> prop { (l: List[String], x: Int) => 
      (x >= 0) ==> {
        Combinator.advance[String](x).parseOnly(l) match {
          case ParseResult.Done(_, ()) => ok
          case _ => ko
        }
      }
    }

    "elem full match" in {
      val c = Combinator.elem[Char]
      val p = (c, c, c).tupled
      val expected = ParseResult.Done(Queue.empty[Char], ('a', 'b', 'c'))
      Parser.parseOnly(p, Queue('a', 'b', 'c')) must_==(expected)
    }

    "elem not enough input" in {
      val c = Combinator.elem[Char]
      val expected = ParseResult.Fail(Queue.empty[Char], Nil, "not enough input")
      Parser.parseOnly(c, Queue.empty) must_==(expected)
    }

    "take done" in {
      val p = Combinator.take[Char](3)
      val expected = ParseResult.Done(Queue('d'), Queue('a', 'b', 'c'))
      Parser.parseOnly(p, Queue('a', 'b', 'c', 'd')) must_==(expected)
    }

    "take partial" in {
      val p = Combinator.take[Char](4)
      val r1 = Parser.parse(p, Queue('a', 'b'))
      val r2 = r1.feedMany(Queue('c', 'd', 'e'))
      r2 must_==(ParseResult.Done(Queue('e'), Queue('a' to 'd':_*)))
    }

    "take fail" in {
      val p = Combinator.take[Char](3)
      val expected = ParseResult.Fail(Queue('a', 'b'), Nil, "not enough input")
      Parser.parseOnly(p, Queue('a', 'b')) must_==(expected)
    }

    "take orElse" in {
      val p = Combinator.take[Char](3) <+> 
        Combinator.take[Char](1)
      println(p)
      val expected = ParseResult.Done(Queue('b'), Queue('a'))
      Parser.parseOnly(p, Queue('a', 'b')) must_==(expected)
    }
    // demandInput
    // ensure
    // wantInput
    // endOfInput
    // discardLeft
    // discardRight
    // andThen
    // orElse/left
    // orElse/right
    // either/left
    // either/right
    // collect
    // cons
    // many
    // many1
    // manyN
    // manyUntil
    // skipMany
    // skipMany1
    // skipManyN
    // sepBy
    // sepBy1
    // choice/1
    // choice/2
    // opt
    // filter
    // count
  }
}