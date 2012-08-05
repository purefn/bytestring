package purefn.bytestring

import ByteString._

import org.specs2._
import org.scalacheck._

class ByteStringSpec extends Specification with ScalaCheck { def is =
  "pack" ! prop { (a: Array[Byte]) ⇒ pack(a).buf.array must_== a }                                            ^
  "++"   ! prop { (a: Array[Byte], b: Array[Byte]) ⇒ (pack(a) ++ pack(b)) must_== pack(Array.concat(a, b)) }  end
}
