package purefn.bytestring
package syntax

import purefn.bytestring.io._

import scalaz._
import effect.IO
import syntax.Ops

import java.io.InputStream

sealed abstract class InputStreamByteStringOps extends Ops[InputStream] {
  final def getContents: IO[ByteString] = sGetContents(self)
  final def getStr(max: Int): IO[ByteString] = sGetStr(self, max)
  final def getLine: OptionT[IO, ByteString] = sGetLine(self)
  final def getLines: StreamT[IO, ByteString] = sGetLines(self)
}

trait ToInputStreamByteStringOps {
  implicit def ToInputStreamByteStringOps[F](s: InputStream) =
    new InputStreamByteStringOps { def self = s }
}
