package purefn.bytestring

import ByteString._
import syntax._
import purefn.bytestring.scalacheck._

import scalaz._
import std.anyVal._
import std.option._
import std.stream._
import std.tuple._
import scalaz.syntax.std.stream._

import org.specs2._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._

import scala.util.control.Exception._

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
  
  def unfoldrGen: Stream[Byte] ⇒ Option[(Byte, Stream[Byte])] = _ match {
    case Stream.Empty ⇒ None
    case h #:: t      ⇒ Some(h, t)
  }

  "unfoldr" ! check { (xs: Stream[Byte]) ⇒
    unfoldr(xs)(unfoldrGen).toStream must be_=== (xs)
  }

  "unfoldrN" ! check { (n: Short, xs: Stream[Byte]) ⇒
    unfoldrN(n, xs)(unfoldrGen)._1.toStream must be_=== (xs.take(n))
  }

  "unzip" ! check { (xs: Stream[(Byte, Byte)]) ⇒
    Bifunctor[Tuple2].umap(unzip(xs))(_.toStream) must be_=== (Bifunctor[Tuple2].umap(xs.unzip)(packF[Stream](_).toStream))
  }

  "++" ! check { (xs: Stream[Byte], ys: Stream[Byte]) ⇒ 
    (packF(xs) ++ packF(ys)) must be_=== (packF(xs ++ ys))
  }

  ":+" ! check { (xs: Stream[Byte], y: Byte) ⇒
    (packF(xs) :+ y) must be_=== (packF(xs :+ y))
  }

  "+:" ! check { (x: Byte, ys: Stream[Byte]) ⇒
    (x +: packF(ys)) must be_=== (packF(x +: ys))
  }

  case class ApplyDatum(xs: Stream[Byte], i: Int)
  implicit def arbApplyDatum = Arbitrary(
    for {
      xs ← listOf1(arbitrary[Byte])
      i  ← choose(0, xs.length - 1)
    } yield ApplyDatum(xs.toStream, i)
  )

  "apply" ! check { (x: ApplyDatum) ⇒
    packF(x.xs).apply(x.i) must be_=== (x.xs(x.i))
  }

  case class BreakDatum(xs: Stream[Byte], f: Byte ⇒ Boolean)
  implicit def arbBreakDataum = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      b  ← oneOf(xs)
    } yield BreakDatum(xs.toStream, _ == b)
  )

  "break" ! check { (x: BreakDatum) ⇒
    packF(x.xs).break(x.f) must be_=== (Bifunctor[Tuple2].umap(x.xs.span(!x.f(_)))(packF[Stream]))
  }
 
  "break(f) === span(!f)" ! check { (x: BreakDatum) ⇒
    val b = packF(x.xs)
    b.break(x.f) must be_=== (b.span(!x.f(_)))
  }

  case class SliceDatum(xs: Stream[Byte], ys: Stream[Byte])
  implicit def arbSliceDatum = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      rs ← listOf(arbitrary[Byte])
      ys ← oneOf(
        for {
          i  ← choose(0, xs.length - 1)
          j  ← choose(i, xs.length - 1)
        } yield xs.slice(i, j)
      , listOf(arbitrary[Byte])
      )
    } yield SliceDatum(xs.toStream, ys.toStream)
  )

  "breakOnSlice" ! check { (x: SliceDatum) ⇒
    def streamBreakOnSlice = x.xs.indexOfSlice(x.ys) match {
      case -1 ⇒ (x.xs, Stream.empty)
      case i  ⇒ x.xs.splitAt(i)
    }
    packF(x.xs).breakOnSlice(packF(x.ys)) must be_=== (Bifunctor[Tuple2].umap(streamBreakOnSlice)(packF[Stream]))
  }

  "containsSlice" ! check { (x: SliceDatum) ⇒
    packF(x.xs).containsSlice(packF(x.ys)) must be_=== (x.xs.containsSlice(x.ys))
  }

  "b === b.copy" ! check { (x: ByteString) ⇒ x must be_=== (x.copy) }

  "drop" ! check { (xs: Stream[Byte], n: Int) ⇒
    packF(xs).drop(n).toStream must be_=== (xs.drop(n))
  }

  case class DropTakeWhileDatum(xs: Stream[Byte], p: Byte ⇒ Boolean)
  implicit def arbDropTakeWhileDatam = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
    } yield DropTakeWhileDatum(xs.toStream, xs.take(n).toSet)
  )

  "dropWhile" ! check { (x: DropTakeWhileDatum) ⇒
    packF(x.xs).dropWhile(x.p).toStream must be_=== (x.xs.dropWhile(x.p))
  }

  case class EndsWithDatum(xs: Stream[Byte], ys: Stream[Byte])
  implicit def arbEndsWithDatum = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
      ys ← oneOf(xs.drop(n), listOf(arbitrary[Byte]))
    } yield EndsWithDatum(xs.toStream, ys.toStream)
  )

  "endsWith" ! check { (x: EndsWithDatum) ⇒
    packF(x.xs).endsWith(packF(x.ys)) must be_=== (x.xs.endsWith(x.ys))
  }

  "exists" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).exists(_ == y) must be_=== (xs.exists(_ == y))
  }

  "filter" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).filter(_ == y).toStream must be_=== (xs.filter(_ == y))
  }

  "find" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).find(_ == y) must be_=== (xs.find(_ == y))
  }

  "foldLeft" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).foldLeft(Stream.empty[Byte])(_ :+ _) must be_=== (xs)
  }

  "foldRight" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).foldRight(Stream.empty[Byte])(_ #:: _) must be_=== (xs)
  }

  case class ForallDatum(xs: Stream[Byte], p: Byte ⇒ Boolean)
  implicit def arbForallDatum = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
    } yield ForallDatum(xs.toStream, xs.take(n).toSet)
  )

  "forall" ! check { (x: ForallDatum) ⇒
    packF(x.xs).forall(x.p) must be_=== (x.xs.forall(x.p))
  }

  "groupBy" ! check { (xs: Stream[Byte]) ⇒
    def streamGroupBy(xs: Stream[Byte]): Stream[Stream[Byte]] =
      xs match {
        case Stream.Empty ⇒ Stream.empty
        case h #:: t      ⇒
          val i = t.indexWhere(_ != h)
          val n = if (i == -1) xs.length else i + 1
          xs.take(n) #:: streamGroupBy(xs.drop(n))
      }
    packF(xs).groupBy(_ == _) must be_=== (streamGroupBy(xs).map(packF[Stream]))
  }

  "head non-empty" ! check { (xs: Stream[Byte] @@ NonEmpty) ⇒
    packF(xs).head must be_=== (xs.head)
  }

  "headOption" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).headOption must be_=== (xs.headOption)
  }

  "indexOf" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).indexOf(y) must be_=== (i2o(xs.indexOf(y)))
  }

  "indexOfSlice" ! check { (x: SliceDatum) ⇒
    packF(x.xs).indexOfSlice(packF(x.ys)) must be_=== (i2o(x.xs.indexOfSlice(x.ys)))
  }

  "indexWhere" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).indexWhere(_ == y) must be_=== (i2o(xs.indexWhere(_ == y)))
  }

  "init non-empty" ! check { (xs: Stream[Byte] @@ NonEmpty) ⇒
    packF(xs).init must be_=== (packF(xs.init))
  }

  "intercalate" ! check { (xss: Stream[Stream[Byte]], ys: Stream[Byte]) ⇒
    xss.map(packF[Stream]).intercalate(packF(ys)) must be_=== (packF(xss.intercalate(ys)))
  }

  "intersperse" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).intersperse(y) must be_=== (packF(xs.intersperse(y)))
  }

  "isEmpty" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).isEmpty must be_=== (xs.isEmpty)
  }

  "last non-empty" ! check { (xs: Stream[Byte] @@ NonEmpty) ⇒
    packF(xs).last must be_=== (xs.last)
  }

  "lastOption" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).lastOption must be_=== (xs.lastOption)
  }

  def i2o(i: Int) = if (i < 0) None else Some(i)

  trait NonEmpty

  implicit def nonEmptyStream[A : Arbitrary]: Arbitrary[Stream[A] @@ NonEmpty] = 
    Arbitrary(containerOf1[Stream, A](arbitrary[A]).map(_.asInstanceOf[Stream[A] @@ NonEmpty]))
}
