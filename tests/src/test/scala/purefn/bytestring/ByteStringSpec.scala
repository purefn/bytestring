package purefn.bytestring

import ByteString._
import syntax._
import purefn.bytestring.scalacheck._

import scalaz._
import std.anyVal._
import std.list._
import std.option._
import std.stream._
import std.string._
import std.tuple._
import scalaz.syntax.bifunctor._
import scalaz.syntax.foldable._
import scalaz.syntax.functor._
import scalaz.syntax.zip._
import scalaz.syntax.std.stream._

import org.specs2._
import org.scalacheck._
import Gen.{choose, listOf, oneOf}
import Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._

import scala.util.control.Exception._

class ByteStringSpec extends Spec {
  // force these case objects to be initialized now to avoid deadlock https://issues.scala-lang.org/browse/SI-6256
  scalaz.Ordering.LT
  scalaz.Ordering.EQ
  scalaz.Ordering.GT

  // Potentially slightly nonsense implementations but should suffice in this case.
  implicit val arrayByteShow: Show[Array[Byte]] = Show.shows(_.toList.toString)
  implicit val arrayByteEqual: Equal[Array[Byte]] = Equal.equalBy(_.toList)

  checkAll("ByteString", order.laws[ByteString])
  checkAll("ByteString", monoid.laws[ByteString])

  import ByteStringSpecFunctions._
  import ByteStringSpecData._

  "singleton" ! check { (x: Byte) ⇒
    singleton(x).toStream must be_=== (Stream(x))
  }

  "pack" ! check { (xs: Stream[Byte]) ⇒ 
    pack(xs.toArray).toStream must be_=== (xs)
  }

  "packs" ! check { (x: String) ⇒
    packs(x).toStream must be_=== (x.getBytes.toStream)
  }

  "packs(str) === pack(str.getBytes)" ! check { (x: String) ⇒
    packs(x) must be_=== (pack(x.getBytes))
  }

  "packF" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).toStream must be_=== (xs)
  }

  "packF(xs.toStream) === pack(xs)" ! check { (xs: Stream[Byte]) ⇒
    (packF(xs)) must be_=== (pack(xs.toArray))
  }

  // use Short so the sizes don't get too huge
  "replicate" ! check { (n: Short, x: Byte) ⇒
    replicate(n, x).toList must be_=== (List.fill(n)(x))
  }
  
  "unfoldr" ! check { (xs: Stream[Byte]) ⇒
    unfoldr(xs)(unfoldrStream).toStream must be_=== (xs)
  }

  "unfoldrN" ! check { (n: Short, xs: Stream[Byte]) ⇒
    unfoldrN(n, xs)(unfoldrStream)._1.toStream must be_=== (xs.take(n))
  }

  "unzip" ! check { (xs: Stream[(Byte, Byte)]) ⇒
    unzip(xs) must be_=== (bipackF(xs.unzip))
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

  "apply" ! check { (x: IndexDatum) ⇒
    if (x.i < 0 || x.i >= x.xs.length) packF(x.xs).apply(x.i) must throwA[RuntimeException]
    else packF(x.xs).apply(x.i) must be_=== (x.xs(x.i))
  }

  "break" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).break(x.eq) must be_=== (bipackF(x.xs.span(!x.eq(_))))
  }
 
  "break(f) === span(!f)" ! check { (x: OneOfDatum) ⇒
    val b = packF(x.xs)
    b.break(x.eq) must be_=== (b.span(!x.eq(_)))
  }

  "breakOnSlice" ! check { (x: SliceDatum) ⇒
    def streamBreakOnSlice = x.xs.indexOfSlice(x.ys) match {
      case -1 ⇒ (x.xs, Stream.empty)
      case i  ⇒ x.xs.splitAt(i)
    }
    packF(x.xs).breakOnSlice(packF(x.ys)) must be_=== (bipackF(streamBreakOnSlice))
  }

  "containsSlice" ! check { (x: SliceDatum) ⇒
    packF(x.xs).containsSlice(packF(x.ys)) must be_=== (x.xs.containsSlice(x.ys))
  }

  "b === b.copy" ! check { (x: ByteString) ⇒ x must be_=== (x.copy) }

  "drop" ! check { (xs: Stream[Byte], n: Short) ⇒
    packF(xs).drop(n).toStream must be_=== (xs.drop(n))
  }

  "dropWhile" ! check { (x: WhileDatum) ⇒
    packF(x.xs).dropWhile(x.p).toStream must be_=== (x.xs.dropWhile(x.p))
  }

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

  "forall" ! check { (x: WhileDatum) ⇒
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

  "head" ! check { (xs: Stream[Byte]) ⇒
    val y = packF(xs)
    if (xs.isEmpty) y.head must throwA[RuntimeException]
    else y.head must be_=== (xs.head)
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

  "init" ! check { (xs: Stream[Byte]) ⇒
    val y = packF(xs)
    if (xs.isEmpty) y.init must throwA[RuntimeException]
    else y.init must be_=== (packF(xs.init))
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

  "last" ! check { (xs: Stream[Byte]) ⇒
    val y = packF(xs)
    if (xs.isEmpty) y.last must throwA[RuntimeException]
    else xs.last must be_=== (xs.last)
  }

  "lastOption" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).lastOption must be_=== (xs.lastOption)
  }

  "lastBreak" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).lastBreak(x.eq) must be_=== (bipackF(x.xs.splitAt(x.xs.lastIndexWhere(x.eq)+1)))
  }

  "lastIndexOf" ! check { (xs: Stream[Byte], y: Byte) ⇒
    packF(xs).lastIndexOf(y) must be_=== (i2o(xs.lastIndexOf(y)))
  }

  "lastSpan" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).lastSpan(x.eq) must be_=== (bipackF(x.xs.splitAt(x.xs.lastIndexWhere(!x.eq(_))+1)))
  }

  "length" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).length must be_=== (xs.length)
  }

  "map" ! check { (xs: Stream[Byte]) ⇒
    def f(x: Byte) = (x + 1).toByte
    packF(xs).map(f) must be_=== (packF(xs.map(f)))
  }

  "partition" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).partition(x.eq) must be_=== (bipackF(x.xs.partition(x.eq)))
  }

  "parition(p) === (filter(p), filter(!p))" ! check { (x: OneOfDatum) ⇒
    val y = packF(x.xs)
    y.partition(x.eq) must be_=== ((y.filter(x.eq), y.filter(!x.eq(_))))
  }

  "reverse" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).reverse must be_=== (packF(xs.reverse))
  }

  "span" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).span(x.eq) must be_=== (bipackF(x.xs.span(x.eq)))
  }

  "span(p) === (takeWhile(p), dropWhile(p))" ! check { (x: OneOfDatum) ⇒
    val y = packF(x.xs)
    y.span(x.eq) must be_=== ((y.takeWhile(x.eq), y.dropWhile(x.eq)))
  }

  "split" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).split(x.y) must be_=== (streamSplitWith(x.xs, x.eq))
  }

  "splitAt" ! check { (x: IndexDatum) ⇒
    packF(x.xs).splitAt(x.i) must be_=== (bipackF(x.xs.splitAt(x.i)))
  }

  "splitAt(i) === (take(i), drop(i))" ! check { (x: IndexDatum) ⇒
    val y = packF(x.xs)
    y.splitAt(x.i) must be_=== (y.take(x.i), y.drop(x.i))
  }

  "splitWith" ! check { (x: OneOfDatum) ⇒
    packF(x.xs).splitWith(x.eq) must be_=== (streamSplitWith(x.xs, x.eq))
  }

  "startsWith" ! check { (x: StartsWithDatum) ⇒
    packF(x.xs).startsWith(packF(x.ys)) must be_=== (x.xs.startsWith(x.ys))
  }
  
  "tail" ! check { (xs: Stream[Byte]) ⇒
    if (xs.isEmpty) packF(xs).tail must throwA[RuntimeException]
    else packF(xs).tail must be_=== (packF(xs.tail))  
  }

  "take" ! check { (xs: Stream[Byte], n: Short) ⇒
    packF(xs).take(n) must be_=== (packF(xs.take(n)))
  }

  "takeWhile" ! check { (x: WhileDatum) ⇒
    packF(x.xs).takeWhile(x.p) must be_=== (packF(x.xs.takeWhile(x.p)))
  }

  "toCord" ! check { (xs: Stream[Byte], y: CharSet) ⇒
    packF(xs).toCord(y) must be_=== (Cord.stringToCord(new String(xs.toArray, y.value)))
  }

  "toList" ! check { (xs: List[Byte]) ⇒
    packF(xs).toList must be_=== (xs)
  }

  "toArray" ! check { (xs: Array[Byte]) ⇒
    pack(xs).toArray must be_=== (xs)
  }

  "toString" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).toString must be_=== (new String(xs.toArray, "UTF-8"))
  }

  "uncons" ! check { (xs: Stream[Byte]) ⇒
    packF(xs).uncons must be_=== (#::.unapply(xs).map(_ :-> packF[Stream]))
  }

  "zip" ! check { (xs: Stream[Byte], ys: Stream[Byte]) ⇒
    packF(xs).zip(packF(ys)) must be_=== (xs.zip(ys))
  }

  "zipWith" ! check { (xs: Stream[Byte], ys: Stream[Byte]) ⇒
    val f = (_:Byte) + (_:Byte)
    packF(xs).zipWith(packF(ys))(f) must be_=== (xs.fzipWith(ys)(f))
  }

  "zipWithBS" ! check { (xs: Stream[Byte], ys: Stream[Byte]) ⇒
    val f: (Byte, Byte) ⇒ Byte = (x, y) ⇒ (x + y).toByte
    packF(xs).zipWithBS(packF(ys))(f) must be_=== (packF(xs.fzipWith(ys)(f)))
  }
}

object ByteStringSpecFunctions {
  def i2o(i: Int) = if (i < 0) None else Some(i)

  def bipackF(xys: (Stream[Byte], Stream[Byte])) = Bifunctor[Tuple2].umap(xys)(packF[Stream])

  def streamSplitWith(xs: Stream[Byte], f: Byte ⇒ Boolean): Stream[ByteString] = {
    def loop(ys: Stream[Byte]): Stream[ByteString] = ys.span(!f(_)) match {
      case (c, Stream.Empty) ⇒ Stream(packF(c))
      case (c, rest)         ⇒ packF(c) #:: loop(rest.tail)
    }
    if (xs.isEmpty) Stream.empty
    else loop(xs)
  }

  def unfoldrStream: Stream[Byte] ⇒ Option[(Byte, Stream[Byte])] = _ match {
    case Stream.Empty ⇒ None
    case h #:: t      ⇒ Some(h, t)
  }
}

object ByteStringSpecData {
  case class IndexDatum(xs: Stream[Byte], i: Int)
  implicit def arbIndexDatum: Arbitrary[IndexDatum] = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      i  ← oneOf(arbitrary[Int], choose(0, xs.length - 1))
    } yield IndexDatum(xs.toStream, i)
  )

  case class OneOfDatum(xs: Stream[Byte], y: Byte) {
    lazy val eq: Byte ⇒ Boolean = _ == y
  }
  implicit def arbOneOfDatum: Arbitrary[OneOfDatum] = Arbitrary {
    for {
      xs ← listOf(arbitrary[Byte])
      b  ← oneOf(arbitrary[Byte], oneOf(xs))
    } yield OneOfDatum(xs.toStream, b)
  }

  case class SliceDatum(xs: Stream[Byte], ys: Stream[Byte])
  implicit def arbSliceDatum: Arbitrary[SliceDatum] = Arbitrary(
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

  case class WhileDatum(xs: Stream[Byte], p: Byte ⇒ Boolean)
  implicit def arbWhileDatam: Arbitrary[WhileDatum] = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
    } yield WhileDatum(xs.toStream, xs.take(n).toSet)
  )

  case class EndsWithDatum(xs: Stream[Byte], ys: Stream[Byte])
  implicit def arbEndsWithDatum: Arbitrary[EndsWithDatum] = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
      ys ← oneOf(xs.drop(n), listOf(arbitrary[Byte]))
    } yield EndsWithDatum(xs.toStream, ys.toStream)
  )

  case class StartsWithDatum(xs: Stream[Byte], ys: Stream[Byte])
  implicit def arbStartsWithDatum: Arbitrary[StartsWithDatum] = Arbitrary(
    for {
      xs ← listOf(arbitrary[Byte])
      n  ← choose(0, xs.length)
      ys ← oneOf(listOf(arbitrary[Byte]), xs.slice(0, n))
    } yield StartsWithDatum(xs.toStream, ys.toStream)
  )

  implicit lazy val arbCharSet: Arbitrary[CharSet] = Arbitrary { 
    import CharSet._
    oneOf(USASCII, ISO8859, UTF8, UTF16BE, UTF16LE, UTF16)
  }

  // TODO remove with next scalaz update
  implicit lazy val CordEqual: Equal[Cord] = new Equal[Cord] {
    def equal(x: Cord, y: Cord) = Equal[Stream[Char]].equal(x.toStream, y.toStream)
  }

  // TODO remove with next scalaz update
  implicit lazy val CordShow: Show[Cord] = new Show[Cord] {
    override def show(x: Cord) = '"' -: x :- '"'
  }
}
