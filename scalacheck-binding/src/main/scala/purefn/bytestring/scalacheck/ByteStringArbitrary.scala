package purefn.bytestring

import org.scalacheck._
import Gen.{choose, containerOfN}
import Arbitrary.arbitrary

package object scalacheck {
  implicit def arbByteString: Arbitrary[ByteString] = Arbitrary(
    for {
      n  ← choose(0, 1024) // 0 - 1M
      xs ← containerOfN[Array, Byte](n, arbitrary[Byte])
    } yield ByteString.pack(xs)
  )
}
