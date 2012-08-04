package purefn.bytestring
package io

import scalaz._
import effect.IO

import java.io.OutputStream
import java.nio.channels.Channels

trait OutputStreamFunctions {
  /** Outputs a `ByteString` to the specified `OutputStream`. */
  def sPutStr(b: ByteString, os: OutputStream): IO[Unit] =
    if (b.buf.hasArray) IO(os.write(b.buf.array, b.buf.arrayOffset + b.buf.position, b.length))
    else IO(Channels.newChannel(os).write(b.buf.duplicate))

  /** Outputs a `ByteString` to the specified `OutputStream`, appending a newline byte. */
  def sPutStrLn(b: ByteString, os: OutputStream): IO[Unit] =
    sPutStr(b, os).flatMap(_ ⇒ sPutStr(ByteString.singleton('\n'.toByte), os))
}
