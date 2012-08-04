package purefn.bytestring
package io

import scalaz._
import effect.IO

import scala.annotation._

import java.io.InputStream
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
      case (bufs, l) ⇒ create(l)(dst ⇒ bufs.map(dst.put))
    })
  }

  /** Read up to `max` bytes into a `ByteString` directly from the specified `InputStream`. */
  def sGetStr(is: InputStream, max: Int): IO[ByteString] = {
    if (max == 0) IO(empty)
    else if (max < 0) illegalBufferSize("sGetStr", max)
    else IO {
      val buf = Array.ofDim[Byte](max)
      val len = is.read(buf, 0, max)
      ByteString(ByteBuffer.wrap(buf, 0, len)).copy
    }
  }

  /** 
   * Read a line from an `InputStream`.
   *
   * Using an `InputStream` that supports `mark` and `reset` is highly recommended.
   * If the `InputStream` supports `mark` and `reset`, bytes can be read in bulk.
   * Otherwise bytes must be read one at a time, which is much less efficient.
   */
  def sGetLine(is: InputStream): IO[ByteString] = {
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
        if (r == -1) len
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
    @tailrec def chunks(n0: Int, n1: Int,  bufs: DList[ByteBuffer], len: Int): (DList[ByteBuffer], Int) = {
      val buf = ByteBuffer.allocate(n0)
      val l = read(buf, 0)
      buf.flip
      if (l == 0) (bufs, len)
      else if (l < n0) (bufs :+ buf, len + l)
      else chunks(n1, n0+n1, bufs :+ buf, len + l)
    }
    IO(chunks(32, 64, DList(), 0) match {
      case (bufs, l) ⇒ create(l)(dst ⇒ bufs.map(dst.put))
    })
  }
}
