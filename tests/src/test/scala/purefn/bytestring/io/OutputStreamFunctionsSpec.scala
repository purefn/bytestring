package purefn.bytestring
package io

import ByteString._
import syntax._
import scalacheck._

import java.io._

class OutputStreamFunctionsSpec extends Spec {
  "sPutStr" ! check { (x: ByteString) ⇒
    val os = new ByteArrayOutputStream(x.length)
    os.putStr(x).unsafePerformIO
    pack(os.toByteArray) must be_=== (x)
  }

  "sPutStrLn" ! check { (x: ByteString) ⇒
    val os = new ByteArrayOutputStream(x.length)
    os.putStrLn(x).unsafePerformIO
    pack(os.toByteArray) must be_=== (x :+ '\n'.toByte)
  }
}
