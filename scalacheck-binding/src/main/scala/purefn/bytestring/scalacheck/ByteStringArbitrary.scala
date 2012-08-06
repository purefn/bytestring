package purefn.bytestring

import org.scalacheck._
import Arbitrary.arbitrary

package object scalacheck {
  implicit def arbByteString: Arbitrary[ByteString] = Arbitrary(arbitrary[Array[Byte]] map ByteString.pack)
}
