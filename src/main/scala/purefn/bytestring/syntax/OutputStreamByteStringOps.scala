package purefn.bytestring
package syntax

import purefn.bytestring.io._

import scalaz._
import effect.IO
import syntax.Ops

import java.io.OutputStream

sealed abstract class OutputStreamByteStringOps extends Ops[OutputStream] {
  final def putStr(b: ByteString): IO[Unit] = sPutStr(b, self)
  final def putStrLn(b: ByteString): IO[Unit] = sPutStrLn(b, self)
}

trait ToOutputStreamByteStringOps {
  implicit def ToOutputStreamByteStringOps[F](s: OutputStream) =
    new OutputStreamByteStringOps { def self = s }
}
