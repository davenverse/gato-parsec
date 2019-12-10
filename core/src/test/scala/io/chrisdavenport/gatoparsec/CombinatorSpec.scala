package io.chrisdavenport.gatoparsec

import io.chrisdavenport.gatoparsec.implicits._
import org.specs2._
import org.specs2.mutable.Specification
import cats.data.Chain
import cats.implicits._

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

    "elem full match" >> {
      val c = Combinator.elem[Char]
      val p = (c, c, c).tupled
      val expected = ParseResult.Done(Chain.empty[Char], ('a', 'b', 'c'))
      Parser.parseOnly(p, Chain('a', 'b', 'c')) must_==(expected)
    }

    "elem not enough input" >> {
      val c = Combinator.elem[Char]
      val expected = ParseResult.Fail(Chain.empty[Char], Nil, "not enough input")
      Parser.parseOnly(c, Chain.empty) must_==(expected)
    }

    "take done" >> {
      val p = Combinator.take[Char](3)
      val expected = ParseResult.Done(Chain.one('d'), Chain('a', 'b', 'c'))
      Parser.parseOnly(p, Chain('a', 'b', 'c', 'd')) must_==(expected)
    }

    "take fail" >> {
      val p = Combinator.take[Char](3)
      val expected = ParseResult.Fail(Chain.empty[Char], Nil, "not enough input")
      Parser.parseOnly(p, Chain('a', 'b')) must_==(expected)
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
