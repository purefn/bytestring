package purefn.bytestring
package io

import ByteString._
import syntax._

import scalaz._
import std.stream._
import syntax.std.stream._

import java.io._

class InputStreamFunctionsSpec extends Spec {
  // TODO clean up with a checkIO ???

  "sGetContents" ! check { (xs: Array[Byte]) ⇒
    val bais = new ByteArrayInputStream(xs)
    bais.getContents.unsafePerformIO must be_=== (pack(xs))
  }

  "sGetStr" ! check { (xs: Array[Byte], y: Int) ⇒
    val bais = new ByteArrayInputStream(xs)
    bais.getStr(y).unsafePerformIO must be_=== (pack(xs.take(y)))
  }

  "sGetLine" ! check { (xss: Stream[Stream[Byte]]) ⇒
    val bais = new ByteArrayInputStream(xss.intercalate(Stream('\n'.toByte)).toArray)

    sGetLine(bais).unsafePerformIO must be_=== (xss.headOption.map(packF[Stream]).getOrElse(empty))
  }
}
