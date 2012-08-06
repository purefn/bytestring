package purefn.bytestring

import ByteString._
import purefn.bytestring.scalacheck._

import scalaz._
import std.anyVal._
import std.stream._
import std.tuple._

import org.specs2._
import org.scalacheck._
import Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._

class ByteStringSpec extends Spec {
  checkAll("ByteString", order.laws[ByteString])
  checkAll("ByteString", monoid.laws[ByteString])

  "singleton" ! check { (x: Byte) ⇒
    singleton(x).buf.array must be_=== (Array(x))
  }

  "pack" ! check { (xs: ByteArray) ⇒ 
    pack(xs.array).buf.array must be_=== (xs.array)
  }

  "packs" ! check { (x: String) ⇒
    packs(x).buf.array must be_=== (x.getBytes)
  }

  "packF" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).buf.array must be_=== (xs.toArray)
  }

  // use Short so the sizes don't get too huge
  "replicate" ! check { (n: Short, x: Byte) ⇒
    replicate(n, x).buf.array must be_=== (Array.fill(n)(x))
  }

  implicit def unfoldrFunctionArb: Arbitrary[Int ⇒ Option[(Byte, Int)]] = Arbitrary {
    for {
      xs ← Gen.listOf(arbitrary[Byte])
      val map = (0 to xs.length).zip(xs.zip(1 to xs.length)).toMap
    } yield map.get(_: Int)
  }
 
  "unfoldr" ! check { (f: Int ⇒ Option[(Byte, Int)]) ⇒
    unfoldr(0)(f).buf.array must be_=== (DList.unfoldr(0, f).toList.toArray)
  }

  "unfoldrN" ! check { (n: Short, f: Int ⇒ Option[(Byte, Int)]) ⇒
    unfoldrN(n, 0)(f)._1.buf.array must be_=== (DList.unfoldr(0, f).toList.take(n).toArray)
  }

  // TODO remove when next version of scalaz is released
  implicit lazy val UnzipStream: Unzip[Stream] = new Unzip[Stream] {
    def unzip[A, B](xs: Stream[(A, B)]) = xs.unzip
  }

  "unzip" ! check { (xs: Stream[(Byte, Byte)]) ⇒
    Bifunctor[Tuple2].umap(unzip(xs))(_.buf.array) must be_=== (Bifunctor[Tuple2].umap(xs.unzip)(packF[Stream](_).buf.array))
  }

  "++" ! check { (x: ByteArray, y: ByteArray) ⇒ 
    (pack(x.array) ++ pack(y.array)) must be_=== (pack(x.array ++ y.array))
  }

  // Used as ScalaCheck in place of Array[Byte] so we have meaningful toString to recreate test cases from
  sealed case class ByteArray(array: Array[Byte]) {
    override def toString = Show[Array[Byte]].shows(array)
  }

  implicit def ByteArrayInstance: Arbitrary[ByteArray] = Arbitrary(arbitrary[Array[Byte]] map (ByteArray(_)))

  implicit def ArrayInstance[A : Equal : Show]: Equal[Array[A]] with Show[Array[A]] = new Equal[Array[A]] with Show[Array[A]] {
    @annotation.tailrec def eq(a: Array[A], b: Array[A], i: Int): Boolean =
      if (i >= a.length) true
      else if (!Equal[A].equal(a(i), b(i))) false
      else eq(a, b, i + 1)

    def equal(a: Array[A], b: Array[A]) =
      if (a.length != b.length) false
      else eq(a, b, 0)

    def show(a: Array[A]) = shows(a).toList

    override def shows(a: Array[A]) = {
      import scala.collection.mutable._
      val buf = new StringBuilder()
      buf ++= "Array("
      var i = 0
      while (i < a.length) {
        buf ++= Show[A].shows(a(i))
        i += 1
      }
      buf += ')'
      buf.toString
    }
  }
}
