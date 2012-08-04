package purefn.bytestring
package syntax

import scalaz._
import syntax._

sealed abstract class StreamByteStringOps extends Ops[Stream[ByteString]] {
  final def intercalate(b: ByteString): ByteString = b.intercalate(self) 
}

trait ToStreamByteStringOps {
  implicit def ToStreamByteStringOps[F](s: Stream[ByteString]) =
    new StreamByteStringOps { def self = s }
}
