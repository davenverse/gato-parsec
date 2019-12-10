package io.chrisdavenport.gatoparsec

import io.chrisdavenport.gatoparsec.implicits._
import org.specs2._
import org.specs2.mutable.Specification
import cats.data.Chain

class CombinatorSpec extends Specification with ScalaCheck {
  "Combinator" should {
    "parse ok correctly always" >> prop { (s: String, n: Int) =>
      Combinator.ok[String](n).parseOnly(Chain.one(s)) match {
        case ParseResult.Done(_, _) => ok
        case _ => ko
      }
    }
    "err should always fail" >> prop {s: String => 
      Combinator.err[String, String]("oops").parseOnly(Chain.one(s)) match {
        case ParseResult.Fail(_, _, _) => ok
        case _ => ko
      }
    }

    "get should always succeed" >> prop { s: String => 
      Combinator.get[String].parseOnly(Chain.one(s)) match {
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

        simpleParser.parseOnly(Chain.fromSeq(l)) match {
          case ParseResult.Done(_, position) => position must_=== (l.length - 1)
          case _ => ko
        }
      }
    }

    "advance should succeed" >> prop { (l: List[String], x: Int) => 
      (x >= 0) ==> {
        Combinator.advance[String](x).parseOnly(Chain.fromSeq(l)) match {
          case ParseResult.Done(_, ()) => ok
          case _ => ko
        }
      }
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