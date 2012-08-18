package purefn.bytestring
package io

import java.io._

object ByteStringInputStream {
  def apply(x: ByteString): InputStream = new BufferedInputStream(new InputStream {
    var i = 0
    def read: Int =
      if (i >= x.length) -1
      else {
	val y = x(i)
	i = i + 1
	b2i(y)
      }
  })
}

