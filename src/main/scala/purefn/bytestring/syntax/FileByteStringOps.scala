package purefn.bytestring
package syntax

import purefn.bytestring.io._

import scalaz._
import effect.IO
import syntax._

import java.io.File

sealed abstract class FileByteStringOps extends Ops[File] {
  final def readBS: IO[ByteString] = readFile(self)
  final def write(b: ByteString): IO[Unit] = writeFile(b, self)
  final def append(b: ByteString): IO[Unit] = appendFile(b, self)
}

trait ToFileByteStringOps {
  implicit def ToFileByteStringOps[F](s: File) =
    new FileByteStringOps { def self = s }
}
