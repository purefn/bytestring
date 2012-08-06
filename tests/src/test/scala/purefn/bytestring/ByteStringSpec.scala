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
    singleton(x).toStream must be_=== (Stream(x))
  }

  "pack" ! check { (xs: Stream[Byte]) ⇒ 
    pack(xs.toArray).toStream must be_=== (xs)
  }

  "packs" ! check { (x: String) ⇒
    packs(x).toStream must be_=== (x.getBytes.toStream)
  }

  "packF" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).toStream must be_=== (xs)
  }

  // use Short so the sizes don't get too huge
  "replicate" ! check { (n: Short, x: Byte) ⇒
    replicate(n, x).toStream must be_=== (Stream.fill(n)(x))
  }

  implicit def unfoldrFunctionArb: Arbitrary[Int ⇒ Option[(Byte, Int)]] = Arbitrary {
    for {
      xs ← Gen.listOf(arbitrary[Byte])
      val map = (0 to xs.length).zip(xs.zip(1 to xs.length)).toMap
    } yield map.get(_: Int)
  }
 
  // TODO remove when next scalaz version is released
  def unfoldStream[A, B](seed: A)(f: A => Option[(B, A)]): Stream[B] =
    f(seed) match {
      case None         => Stream.empty
      case Some((b, a)) => Stream.cons(b, unfoldStream(a)(f))
    }

  "unfoldr" ! check { (f: Int ⇒ Option[(Byte, Int)]) ⇒
    unfoldr(0)(f).toStream must be_=== (unfoldStream(0)(f))
  }

  "unfoldrN" ! check { (n: Short, f: Int ⇒ Option[(Byte, Int)]) ⇒
    unfoldrN(n, 0)(f)._1.toStream must be_=== (unfoldStream(0)(f).take(n))
  }

  // TODO remove when next version of scalaz is released
  implicit lazy val UnzipStream: Unzip[Stream] = new Unzip[Stream] {
    def unzip[A, B](xs: Stream[(A, B)]) = xs.unzip
  }

  "unzip" ! check { (xs: Stream[(Byte, Byte)]) ⇒
    Bifunctor[Tuple2].umap(unzip(xs))(_.toStream) must be_=== (Bifunctor[Tuple2].umap(xs.unzip)(packF[Stream](_).toStream))
  }

  "++" ! check { (xs: Stream[Byte], ys: Stream[Byte]) ⇒ 
    (packF(xs) ++ packF(ys)) must be_=== (packF(xs ++ ys))
  }
}
