package purefn.bytestring
package io

import scalaz._
import effect.IO

import scala.annotation._

import java.io.{BufferedInputStream, InputStream}
import java.nio.ByteBuffer

trait InputStreamFunctions {
  import ByteString._

  /** 
   * Read entire `InputStream` contents into a `ByteString`.
   *
   * This function reads chunks at a time, doubling the chunksize on each
   * read. The final buffer is then realloced to the appropriate size. For
   * files &gt; half of available memory, this may lead to memory exhaustion.
   *  Consider using `readFile` in this case.
   */
  def sGetContents(is: InputStream): IO[ByteString] = {
    @tailrec def chunks(n: Int, bufs: DList[ByteBuffer], len: Int): (DList[ByteBuffer], Int) = {
      val buf = Array.ofDim[Byte](n)
      is.read(buf) match {
        case l if l == -1 ⇒ (bufs, len)
        case l if l < n   ⇒ (bufs :+ ByteBuffer.wrap(buf, 0, l), len + l) // avoid an extra alloc
        case l            ⇒ chunks(n * 2, bufs :+ ByteBuffer.wrap(buf), len + l)
      }
    }
    IO(chunks(1024, DList(), 0) match {
      case (bufs, l) ⇒ create(l) { dst ⇒ bufs.map(dst.put); () }
    })
  }

  /** Read up to `max` bytes into a `ByteString` directly from the specified `InputStream`. */
  // TODO Should this return `IO[Option[ByteString]]` or `OptionT[IO, ByteString]` with
  //      `None` indicating EOF has been reached? There isn't any other way to check
  //      that with `InputStream`s.
  def sGetStr(is: InputStream, max: Int): IO[ByteString] = {
    if (max <= 0) IO(empty)
    else IO {
      val buf = Array.ofDim[Byte](max)
      val len = is.read(buf, 0, max)
      if (len == -1) empty
      else ByteString(ByteBuffer.wrap(buf, 0, len)).copy
    }
  }

  /** 
   * Read a line from an `InputStream`.
   *
   * Using an `InputStream` that supports `mark` and `reset` is highly recommended.
   * If the `InputStream` supports `mark` and `reset`, bytes can be read in bulk.
   * Otherwise bytes must be read one at a time, which is much less efficient.
   */
  def sGetLine(is: InputStream): OptionT[IO, ByteString] = {
    @tailrec def findEol(buf: ByteBuffer, i: Int): Int =
      if (i >= buf.remaining) -1
      else if (buf.get(i) == '\n'.toByte) i
      else findEol(buf, i + 1)
    @tailrec def readOne(buf: ByteBuffer, l: Int): Int = 
      if (buf.remaining == 0) l
      else {
        val b = is.read
        if (b == -1 || b == '\n'.toInt) l
        else readOne(buf.put(i2b(b)), l+1)
      }
    @tailrec def readBulk(buf: ByteBuffer, len: Int): Int =
      if (buf.remaining == 0) len
      else {
        is.mark(buf.remaining)
        val r = is.read(buf.array, buf.position, buf.remaining)
        if (r == -1) 
          if (len == 0) -1
          else len
        else {
          val eol = findEol(buf, buf.position)
          if (eol == -1) {
            buf.position(buf.position + r)
            readBulk(buf, len + r)
          } else {
            buf.position(buf.position + eol)
            is.reset
            is.skip(eol + 1)
            len + eol
          }
        }
      }
    def read(buf: ByteBuffer, l: Int): Int =
      if (is.markSupported) readBulk(buf, l)
      else readOne(buf, l)
    @tailrec def chunks(n0: Int, n1: Int,  bufs: DList[ByteBuffer], bufsLen: Int, len: Int): (DList[ByteBuffer], Int) = {
      val buf = ByteBuffer.allocate(n0)
      val l = read(buf, 0)
      buf.flip
      if (l == -1)
        if (bufsLen == 0) (bufs, -1)
        else (bufs, len)
      else if (l == 0) (bufs, len)
      else if (l < n0) (bufs :+ buf, len + l)
      else chunks(n1, n0+n1, bufs :+ buf, bufsLen + 1, len + l)
    }
    OptionT(IO(chunks(32, 64, DList(), 0, 0) match {
      case (_, -1)   ⇒ None
      case (bufs, l) ⇒ 
        def put(bufs: DList[ByteBuffer], dst: ByteBuffer): Unit =
          bufs.uncons((), (buf, rest) ⇒ { dst.put(buf); put(rest, dst) })
        Some(create(l)(put(bufs, _)))
    }))
  }

  /** Read all lines from an `InputStream`. */
  def sGetLines(is: InputStream): StreamT[IO, ByteString] = {
    def buffered =
      if (is.markSupported) is
      else new BufferedInputStream(is)
    StreamT.unfoldM(buffered)(is0 ⇒ sGetLine(is0).map((_, is0)).run)
  }
}
