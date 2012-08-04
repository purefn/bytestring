package purefn.bytestring

import scalaz._
import effect.IO

import java.io._

package object io extends InputStreamFunctions with OutputStreamFunctions with FileFunctions {
  /**
   * Read stdin. Equivalent to
   *
   * &gt; sGetContents(System.in)
   */
  def getContents: IO[ByteString] = sGetContents(System.in)

  /** Read a line from stdin. */
  def getLine: IO[ByteString] = sGetLine(System.in)

  /** Write a `ByteString` to stdout */
  def putStr(b: ByteString): IO[Unit] = sPutStr(b, System.out)

  /** Write a `ByteString` to stdout, appending a newline byte */
  def putStrLn(b: ByteString): IO[Unit] = sPutStrLn(b, System.out)

  private[io] def i2b(i: Int): Byte = 
    (if (i > Byte.MaxValue) i - 256 else i).toByte

  private[io] def illegalBufferSize[A](fn: String, n: Long): IO[A] =
    IO.throwIO(new IOException(fn + ": illegal ByteString size " + n))
}
