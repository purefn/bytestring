package purefn.bytestring

import scalaz._
import std.anyVal._
import std.stream._
import std.tuple._

import scala.annotation._

import java.nio.ByteBuffer
import java.nio.charset.Charset

sealed abstract class ByteString(private[bytestring] val buf: ByteBuffer) { 
  import ByteString._

  /**
   * Append two `ByteString`s.
   */
  final def ++(b: ByteString): ByteString =
    if (isEmpty) b
    else if (b.isEmpty) this
    else concat(Stream(this, b))

  /**
   * Append a byte to the end of a `ByteString`.
   */
  final def :+(b: Byte): ByteString = withBuf(src ⇒ create(length + 1) { dst ⇒ dst.put(src).put(b); () })

  /**
   * Prepend a byte to the beginning of a `ByteString`.
   */
  final def +:(b: Byte): ByteString = withBuf(src ⇒ create(length + 1) { dst ⇒ dst.put(b).put(src); () })

  /**
   * Get the byte at position `index`.
   */
  final def apply(index: Int): Byte = 
    if (index < 0) error("apply", "negative index: " + index)
    else if (index >= length) error("apply", "index too large: " + index)
    else unsafeApply(index)

  /**
   * Equivalent to `span(!p(_))`.
   */
  /* TODO Would be nice to specialize this with macros for the (_==b) case if we were using
     a direct buffer and had a memchr function */
  final def break(p: Byte ⇒ Boolean): (ByteString, ByteString) = splitAt(findIndexOrEndWhere(p))

  /**
   * Break on a slice
   */
  final def breakOnSlice(b: ByteString): (ByteString, ByteString) = {
    val i = findIndexOfSlice(b)
    if (i < 0) (this, empty)
    else (unsafeTake(i), unsafeDrop(i))
  }

  /**
   * Tests whether this `ByteString` contains a given `ByteString`.
   */
  final def containsSlice(b: ByteString): Boolean = findIndexOfSlice(b) >= 0

  /**
   * Make a copy of this `ByteString` with its own storage. 
   * This is mainly useful to allow the rest of the data pointed
   * to by this `ByteString` to be garbage collected, for example
   * if a large string has been read in, and only a small part of it 
   * is needed in the rest of the program.
   *
   * If there is no data to be garbage collected, this `ByteString` is
   * returned as-is.
   */
  final def copy: ByteString = 
    if (length == buf.capacity) this
    else withBuf(src ⇒ create(length) { dst ⇒ dst.put(src); () })

  /**
   * Extract the suffix after the first `n` elements, or `empty` if `n > length`.
   */
  final def drop(n: Int): ByteString =
    if (n <= 0) this
    else if (n >= length) empty
    else unsafeDrop(n)

  /**
   * Extract the suffix remaining after `takeWhile p`.
   */
  final def dropWhile(p: Byte ⇒ Boolean): ByteString = unsafeDrop(findIndexOrEndWhere(!p(_)))

  /**
   * Tests whether this `ByteString` ends with the given `ByteString`.
   */
  final def endsWith(b: ByteString): Boolean =
    if (b.length == 0) true
    else if (length < b.length) false
    else withBuf(buf ⇒ buf.position(buf.limit - b.length) == b.buf)
  /**
   *  Determines if any element of the `ByteString` satisfies the predicate.
   */
  final def exists(f: Byte ⇒ Boolean): Boolean = findIndexOfWhere(f, 0) >= 0

  /**
   * Returns a ByteString containing those bytes that satisfy the predicate.
   */
  final def filter(p: Byte ⇒ Boolean): ByteString = {
    @tailrec def loop(dst: ByteBuffer, i: Int, l: Int): Int =
      if (i >= length) l
      else {
        val b = unsafeApply(i)
        if (p(b)) loop(dst.put(b), i + 1, l + 1)
        else loop(dst, i + 1, l)
      }
    if (isEmpty) this
    else createAndTrim(length)(loop(_, 0, 0))
  }

  /**
   * Returns the first element in matching the predicate, or `None` if there is no such element.
   */
  final def find(p: Byte ⇒ Boolean): Option[Byte] = indexWhere(p) map unsafeApply

  /**
   * `foldLeft`, applied to a binary operator, a starting value (typically, the left-identity of the operator), 
   * reduces the `ByteString` using the binary operator, from left to right.
   */
  final def foldLeft[A](a: A)(f: (A, Byte) ⇒ A): A = {
    @tailrec def loop(z: A, src: ByteBuffer, n: Int): A =
      if (n > 0) loop(f(z, src.get), src, n - 1)
      else z
    withBuf(loop(a, _, length))
  }

  /**
   * `foldRight`, applied to a binary operator, a starting value (typically the right-identity of the operator),
   * reduces the `ByteString` using the binary operator, from right to left.
   */
  final def foldRight[A](a: A)(f: (Byte, A) ⇒ A): A = {
    @tailrec def loop(z: A, i: Int): A =
      if (i >= 0) loop(f(apply(i), z), i - 1)
      else z
    loop(a, length - 1)
  }

  final def forall(f: Byte ⇒ Boolean): Boolean = {
    @tailrec def loop(src: ByteBuffer, n: Int): Boolean =
      if (n <= 0) true
      else if (f(src.get)) loop(src, n - 1)
      else false
    withBuf(loop(_, length))
  }

  /**
   * Returns a list of `ByteString`s such that the concatenation of the result is equal to the
   * argument.  Moreover, each sublist in the result contains only 
   * elements under the `eq` function.  For example,
   *
   * &gt;  "Mississippi".groupBy(_ == _)  = Stream("M","i","ss","i","ss","i","pp","i")
   */
  final def groupBy(eq: (Byte, Byte) ⇒ Boolean): Stream[ByteString] =
    if (isEmpty) Stream.empty
    else {
      val n = 1 + unsafeTail.findIndexOrEndWhere(!eq(unsafeHead, _))
      unsafeTake(n) #:: unsafeDrop(n).groupBy(eq)
    }

  /**
   * Extract the first element of a `ByteString`, which must be non-empty.
   * An exception will be thrown in the case of an empty `ByteString`.
   */
  final def head: Byte =
    if (isEmpty) errorEmpty("head")
    else unsafeHead

  /** 
   * Gives the index of the first element in this `ByteString` which is equal to `b`,
   * or `None` if there is no such element. 
   */
  final def indexOf(b: Byte): Option[Int] = {
    val i = findIndexOf(b, 0)
    if (i < 0) None
    else Some(i)
  }

  /**
   * Finds first index where this `ByteString` contains a given `ByteString` as a slice.
   */
  final def indexOfSlice(b: ByteString): Option[Int] = {
    val i = findIndexOfSlice(b)
    if (i < 0) None
    else Some(i)
  }

  /**
   * Finds the index of the first element in the ByteString satisfying the predicate.
   */
  final def indexWhere(p: Byte ⇒ Boolean): Option[Int] = {
    val i = findIndexOfWhere(p, 0)
    if (i < 0) None
    else Some(i)
  }

  /**
   * Return all the elements of a `ByteString` except the last one.
   * An exception will be thrown in the case of an empty `ByteString`.
   */
  final def init: ByteString =
    if (isEmpty) errorEmpty("init")
    else withBuf { buf ⇒
      buf.limit(buf.limit - 1)
      ByteString(buf)
    }

  /**
   * Concatenates a list of `ByteString`s after interspersing this `ByteString` argument between each element of the 
   * list.
   */
  /* TODO Specialize with a macro for when `bs.length == 2` and this is a singleton. */
  final def intercalate(bs: Stream[ByteString]): ByteString = concat(streamIntersperse(bs, this))

  /** TODO remove when next version of scalaz is released */
  private[this] def streamIntersperse[A](as: Stream[A], a: A): Stream[A] = {
    def loop(rest: Stream[A]): Stream[A] = rest match {
      case Stream.Empty => Stream.empty
      case h #:: t      => a #:: h #:: loop(t)
    }
    as match {
      case Stream.Empty => Stream.empty
      case h #:: t      => h #:: loop(t)
    }
  }

  /**
   * `intersperse`s a `Byte` between the elements of this `ByteString`.  It is analogous to the intersperse function 
   * on Lists.
   */
  final def intersperse(b: Byte): ByteString = {
    @tailrec def loop(dst: ByteBuffer, i: Int) {
      if (i == length - 1) { dst.put(unsafeApply(i)); () }
      else loop(dst.put(unsafeApply(i)).put(b), i + 1)
    }
    if (length < 2) this
    else create(2 * length - 1)(loop(_, 0))
  }

  /**
   * Test whether the `ByteString` is empty.
   */
  final def isEmpty: Boolean = length <= 0

  /**
   * Extract the last element of a `ByteString`, which must be finite and non-empty.
   * An exception will be thrown in the case of an empty `ByteString`.
   */
  final def last: Byte =
    if (isEmpty) errorEmpty("last")
    else apply(length - 1)

  /**
   * `lastBreak` behaves like `break` but from the end of the `ByteString`.
   */
  final def lastBreak(p: Byte ⇒ Boolean): (ByteString, ByteString) = splitAt(findFromEndUntil(p))

  /**
   * 
   */
  final def lastIndexOf(b: Byte): Option[Int] = {
    val i = findLastIndexOf(b, length - 1)
    if (i < 0) None
    else Some(i)
  }
    
  /**
   * `lastSpan` behaves like `span` but from the end of the `ByteString`.
   */
  final def lastSpan(p: Byte ⇒ Boolean): (ByteString, ByteString) = splitAt(findFromEndUntil(!p(_)))

  /**
   * Length of the `ByteString`.
   */
  final lazy val length: Int = buf.remaining

  /**
   *  The `ByteString` obtained by applying `f` to each element of this `ByteString`.
   */
  final def map(f: Byte ⇒ Byte): ByteString = {
    @tailrec def loop(src: ByteBuffer, dst: ByteBuffer, n: Int) {
      if (n > 0) loop(src, dst.put(f(src.get)), n - 1)
    }
    withBuf(src ⇒ create(length)(loop(src, _, length)))
  }

  /**
   * Returns the pair of `ByteString`s with elements which do and do not satisfy the
   * predicate, respectively; i.e.,
   *
   * &gt; bs.partition(p) == (bs.filter(p), bs.filter(!p(_)))
   */
  final def partition(p: Byte ⇒ Boolean): (ByteString, ByteString) = (filter(p), filter(!p(_)))

  /**
   * Returns a `ByteString` whose elements are in the reverse order of this `ByteString`.
   */
  final def reverse: ByteString = {
    @tailrec def loop(dst: ByteBuffer, i: Int) {
      if (i >= 0) loop(dst.put(apply(i)), i - 1)
    }
    create(length)(loop(_, length - 1))
  }

  /**
   * Breaks the `ByteString` into two segments. Equivalent to `(takeWhile(p), dropWhile(p)`.
   */
  /* TODO Would be nice to specialize this with macros for the (_==b) case if we were using
     a direct buffer and had a memchr function */
  final def span(p: Byte ⇒ Boolean): (ByteString, ByteString) = break(!p(_))

  /**
   * Break a `ByteString` into pieces separated by the byte argument, consuming the delimiter. I.e.
   *
   * &gt; "a\nb\nd\ne" split '\n'  == Stream("a","b","d","e")
   * &gt; "aXaXaXa" split 'a'      == Stream("","X","X","X","")
   * &gt; "x" split 'x'            == Stream("","")
   *
   * As for all splitting functions in this library, this function does not copy the substrings, it just constructs 
   * new 'ByteStrings' that are slices of the original.
   */
  final def split(b: Byte): Stream[ByteString] = {
    def loop(i: Int): Stream[ByteString] = {
      val ii = findIndexOf(b, i)
      if (ii < 0) Stream(slice(i, length))
      else slice(i, ii) #:: loop(ii + 1)
    }
    loop(0)
  }

  /**
   * `splitAt(n)` is equivalent to `(take(n), drop(n))`
   */
  final def splitAt(n: Int): (ByteString, ByteString) =
    if (n <= 0) (empty, this)
    else if (n >= length) (this, empty)
    else (unsafeTake(n), unsafeDrop(n))

  /**
   * Splits a `ByteString` into components delimited by separators, where the predicate returns `true` for a separator 
   * element. The resulting components do not contain the separators.  Two adjacent separators result in an empty
   * component in the output.
   *
   * &gt; "aabbaca".splitWith(_ == 'a')  == Stream("","","bb","c","")
   * &gt; "".splitWith(_ == 'a')         == Stream()
   */
  final def splitWith(p: Byte ⇒ Boolean): Stream[ByteString] = {
    def loop(i: Int, j: Int): Stream[ByteString] =
      if (i + j == length) Stream(slice(i, j))
      else {
        val b = unsafeApply(i + j)
        if (p(b)) slice(i, j) #:: loop(i + j + 1, 0)
        else loop(i, j + 1)
      }
    loop(0, 0)
  }

  /**
   * Tests whether this starts with the give `ByteString`.
   */
  final def startsWith(b: ByteString): Boolean =
    if (b.length == 0) true
    else if (length < b.length) false
    else withBuf(buf ⇒ buf.limit(buf.position + b.length) == b.buf)

  /**
   * Extract the elements after the head of a `ByteString`, which must be non-empty.
   * An exception will be thrown in the case of an empty `ByteString`.
   */
  final def tail: ByteString = 
    if (isEmpty) errorEmpty("tail")
    else unsafeTail

  /**
   * Extract prefix of length `n`, or the `ByteString` itself if `n > length`.
   */
  final def take(n: Int): ByteString =
    if (n <= 0) empty
    else if (n >= length) this
    else unsafeTake(n)

  /**
   * Extracts  the longest prefix (possibly empty) of  elements that satisfy `p`.
   */
  final def takeWhile(p: Byte ⇒ Boolean): ByteString = unsafeTake(findIndexOrEndWhere(!p(_)))

  /**
   * Converts the `ByteString` to a `String` using UTF-8.
   */
  final override lazy val toString = withBuf(b ⇒ Charset.forName("UTF-8").decode(b).toString)

  /**
   * Extract the head and tail of a `ByteString`, returning `None` if it is empty.
   */
  final def uncons: Option[(Byte, ByteString)] = 
    if (isEmpty) None
    else withBuf { buf ⇒
      buf.position(buf.position + 1)
      Some((apply(0), ByteString(buf)))
    }

  /**
   * Returns a list of corresponding pairs of bytes. If one input `ByteString` is short,
   * excess elements of the longer `ByteString` are discarded. This is equivalent to a pair of `unpack` operations.
   */
  final def zip(b: ByteString): Stream[(Byte, Byte)] = zipWith(b)((_, _))

  /**
   * Generalises `zip` by zipping with the function given instead of a tupling function.
   */
  final def zipWith[A](b: ByteString)(f: (Byte, Byte) ⇒ A): Stream[A] =
    if (isEmpty || b.isEmpty) Stream.empty
    else f(unsafeHead, b.unsafeHead) #:: unsafeTail.zipWith(b.unsafeTail)(f)

  /**
   * A specialised version of zipWith for the common case of a simultaneous map over two bytestrings, to build a 3rd.
   */
  final def zipWithBS(b: ByteString)(f: (Byte, Byte) ⇒ Byte): ByteString = {
    val len = math.min(length, b.length)
    @tailrec def loop(dst: ByteBuffer, i: Int) {
      if (i < len) loop(dst.put(f(unsafeApply(i), b.unsafeApply(i))), i + 1)
    }
    create(len)(loop(_, 0))
  }

  @inline final def unsafeApply(index: Int): Byte = buf.get(buf.position + index)

  @inline final def unsafeDrop(n: Int): ByteString = ByteString(withBuf { buf ⇒ buf.position(buf.position + n); buf })

  @inline final def unsafeHead: Byte = unsafeApply(0)

  @inline final def unsafeTail: ByteString = ByteString(withBuf { buf ⇒ buf.position(buf.position + 1); buf })

  @inline final def unsafeTake(n: Int): ByteString = ByteString(withBuf { buf ⇒ buf.limit(buf.position + n); buf })

  /* TODO Would be nice to replace this with something more like memchr if we're using a direct buffer */
  @inline private[bytestring] def findIndexOf(b: Byte, i: Int): Int = findIndexOfWhere(_ == b, i)

  /* TODO Replace with KMP implementation */
  @inline private[bytestring] def findIndexOfSlice(b: ByteString): Int = {
    @tailrec def find(i: Int, src: ByteString): Int =
      if (src.isEmpty) -1
      else if (src startsWith b) i
      else find(i + 1, src.unsafeTail)
    find(0, this)
  }

  @inline private[bytestring] def findIndexOfWhere(p: Byte ⇒ Boolean, i: Int): Int = {
    @tailrec def loop(ii: Int): Int = 
      if (ii >= length) -1
      else if (p(unsafeApply(ii))) ii
      else loop(ii + 1)
    loop(i)
  }

  @inline private[bytestring] def findIndexOrEndWhere(p: Byte ⇒ Boolean): Int = {
    @tailrec def loop(i: Int): Int = 
      if (i >= length) length
      else if (p(unsafeApply(i))) i
      else loop(i + 1)
    loop(0)
  }

  @inline private[bytestring] def findFromEndUntil(p: Byte ⇒ Boolean): Int = {
    @tailrec def loop(i: Int): Int =
      if (i < 0) 0
      else if (p(unsafeApply(i))) i + 1
      else loop(i - 1)
    if (isEmpty) 0
    else loop(length - 1)
  }

  @tailrec final private[bytestring] def findLastIndexOf(b: Byte, i: Int): Int =
    if (i < 0) -1
    else if (unsafeApply(i) == b) i
    else findLastIndexOf(b, i - 1)

  @inline private[bytestring] def slice(i:Int, l: Int) = ByteString(withBuf { buf ⇒ buf.position(buf.position + i).limit(buf.position + l); buf })

  @inline private[bytestring] def withBuf[A](f: ByteBuffer ⇒ A): A = f(buf.duplicate)
}

object ByteString extends ByteStringFunctions with ByteStringInstances

trait ByteStringFunctions {
  private[bytestring] def apply(b: ByteBuffer): ByteString = new ByteString(b) {}

  def concat[F[_]: Foldable](bs: F[ByteString]): ByteString = {
    def len = Foldable[F].foldMap(bs)(_.length)
    @tailrec def loop(xs: Stream[ByteString], buf: ByteBuffer) {
      xs match {
        case Stream.Empty ⇒ ()
        case x #:: ys     ⇒ loop(ys, x.withBuf(buf.put))
      }
    }
    Foldable[F].toStream(bs) match {
      case b #:: Stream.Empty ⇒ b
      case bs                 ⇒ create(len)(loop(bs, _))
    }
  }

  def empty: ByteString = ByteString(ByteBuffer.allocate(0))

  def singleton(b: Byte): ByteString = ByteString(ByteBuffer.wrap(Array(b)))

  def pack(bs: Array[Byte]): ByteString = {
    val buf = ByteBuffer.allocate(bs.length).put(bs)
    buf.flip
    ByteString(buf)
  }

  @inline def packs(s: String): ByteString = pack(s.getBytes)

  def packF[F[_]: Foldable](bs: F[Byte]): ByteString =
    unfoldr(Foldable[F].toStream(bs)) {
      case Stream.Empty ⇒ None
      case h #:: t      ⇒ Some((h, t))
    }

  def replicate(n: Int, b: Byte): ByteString = {
    @tailrec def loop(buf: ByteBuffer, i: Int): ByteString = 
      if (n > i) loop(buf.put(i, b), i + 1)
      else ByteString({ buf.flip; buf })
    if (n < 0) empty
    else loop(ByteBuffer.allocate(n), 0)
  }
 
  def unfoldr[A](a: A)(f: A ⇒ Option[(Byte, A)]): ByteString = {
    @tailrec def chunk(n0: Int, n1: Int, x: A, cs: Stream[ByteString]): Stream[ByteString] = unfoldrN(n0, x)(f) match {
      case (s, None)    ⇒ s #:: cs
      case (s, Some(y)) ⇒ chunk(n1, n0+n1, y, s #:: cs)
    }
    concat(chunk(32, 64, a, Stream.empty))
  }

  def unfoldrN[A](max: Int, seed: A)(f: A ⇒ Option[(Byte, A)]): (ByteString, Option[A]) = {
    @tailrec def loop(buf: ByteBuffer, x: A, n: Int): (Int, Int, Option[A]) = f(x) match {
      case None                     ⇒ (0, n, None)
      case Some((b, y)) if n == max ⇒ (0, n, Some(x))
      case Some((b, y))             ⇒ loop(buf.put(b), y, n + 1)
    }
    if (max < 0) (empty, Some(seed))
    else createAndTrim1(max)(loop(_, seed, 0))
  }

  /**
   * Transforms a list of pairs of bytes into a pair of `ByteString`s. Note that this performs two 'pack' operations.
   */
  def unzip[F[_] : Unzip : Foldable](bs: F[(Byte, Byte)]): (ByteString, ByteString) =
    Bifunctor[Tuple2].umap(Unzip[F]unzip(bs))(packF[F])

  /**
   * Create ByteString of size `n` and use `f` to fill it's contents.
   */
  private[bytestring] def create(n: Int)(f: ByteBuffer ⇒ Unit): ByteString = 
    if (n == 0) empty
    else {
      val buf = ByteBuffer.allocate(n)
      f(buf)
      ByteString({ buf.flip; buf })
    }

  /**
   * Given the maximum size needed and a function to make the contents of a ByteString, createAndTrim makes the 
   * `ByteString`. The generating function is required to return the actual final size (&lt;= the maximum
   * size), and the resulting byte array is realloced to this size.
   */
  private[bytestring] def createAndTrim(n: Int)(f: ByteBuffer ⇒ Int): ByteString = {
    val buf0 = ByteBuffer.allocate(n)
    val l = f(buf0)
    buf0.flip
    if (l >= n) ByteString(buf0)
    else create(l) { buf1 ⇒ 
      buf0.limit(l)
      buf1.put(buf0)
      ()
    }
  }

  private[bytestring] def createAndTrim1[A](n: Int)(f: ByteBuffer ⇒ (Int, Int, A)): (ByteString, A) = {
    val buf0 = ByteBuffer.allocate(n)
    val (off, l, a) = f(buf0)
    buf0.flip
    if (l >= n) (ByteString(buf0), a)
    else {
      val bs = create(l) { buf1 ⇒ 
        buf0.position(off).limit(off + l)
        buf1.put(buf0)
        ()
      }
      (bs, a)
    }
  }

  private[bytestring] def errorEmpty(fun: String) = error(fun, "empty ByteString")

  private[bytestring] def error(fun: String, msg: String) = sys.error("purefun.bytestring.ByteString." + fun + ": " + msg)
}

trait ByteStringInstances {
  implicit lazy val ByteStringOrder: Order[ByteString] = new Order[ByteString] {
    def order(a: ByteString, b: ByteString) = Ordering.fromInt(a.withBuf(buf ⇒ b.withBuf(buf compareTo _)))
  }

  implicit lazy val ByteStringShow: Show[ByteString] = new Show[ByteString] {
    def show(b: ByteString) = shows(b).toList
    override def shows(b: ByteString) = b.toString
  }

  implicit lazy val ByteStringMonoid: Monoid[ByteString] = new Monoid[ByteString] {
    def zero = ByteString.empty
    def append(a: ByteString, b: ⇒ ByteString) = a ++ b
  }
}

