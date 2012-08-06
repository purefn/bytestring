package purefn.bytestring

import ByteString._
import purefn.bytestring.scalacheck._

import scalaz._
import std.anyVal._
import std.stream._

import org.specs2._
import org.scalacheck._
import scalaz.scalacheck.ScalazProperties._

class ByteStringSpec extends Spec {
  checkAll("ByteString", order.laws[ByteString])
  checkAll("ByteString", monoid.laws[ByteString])

  "singleton" ! check { (b: Byte) ⇒
    singleton(b).buf.array must be_=== (Array(b))
  }

  "pack" ! check { (bs: Array[Byte]) ⇒ 
    pack(bs).buf.array must be_=== (bs)
  }

  "packs" ! check { (s: String) ⇒
    packs(s).buf.array must be_=== (s.getBytes)
  }

  "packF" ! check { (bs: Stream[Byte]) ⇒
    packF(bs).buf.array must be_=== (bs.toArray)
  }

  "replicate" ! check { (n: Short, b: Byte) ⇒
    replicate(n, b).buf.array must be_=== (Array.fill(n)(b))
  }

  "++" ! check { (a: Array[Byte], b: Array[Byte]) ⇒ 
    (pack(a) ++ pack(b)) must be_=== (pack(Array.concat(a, b)))
  }

  implicit def ArrayInstance[A : Equal : Show]: Equal[Array[A]] with Show[Array[A]] = new Equal[Array[A]] with Show[Array[A]] {
    @annotation.tailrec def eq(a: Array[A], b: Array[A], i: Int): Boolean =
      if (i >= a.length) true
      else if (!Equal[A].equal(a(i), b(i))) false
      else eq(a, b, i + 1)

    def equal(a: Array[A], b: Array[A]) =
      if (a.length != b.length) false
      else eq(a, b, 0)

    def show(a: Array[A]) = a.toList.flatMap(Show[A].show)
  }
}
