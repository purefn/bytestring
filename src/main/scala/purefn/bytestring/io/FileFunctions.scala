package purefn.bytestring
package io

import scalaz._
import effect.IO

import scala.annotation._

import java.io.{File, FileInputStream, FileOutputStream}

trait FileFunctions {
  /**
   * Read an entire file into a `ByteString`.  This is far more efficient than reading the into a byte array and using
   * `pack`.
   *
   * Note: Due to current implementation limitations, cannot read a file larger than `Int.MaxValue` bytes (2048MB).
   */
  def readFile(f: File): IO[ByteString] =
    if (f.length > Int.MaxValue) illegalBufferSize("readFile", f.length)
    else openFIS(f).bracket(closeFIS)(sGetStr(_, f.length.toInt))

  /** Write a `ByteString` to a file. */
  def writeFile(b: ByteString, f: File): IO[Unit] = writeOrAppendFile(b, f, false)
 
  /** Append a `ByteString` to a file. */
  def appendFile(b: ByteString, f: File): IO[Unit] = writeOrAppendFile(b, f, true)

  private[this] def closeFIS(fis: FileInputStream) = IO(fis.close)

  private[this] def closeFOS(fos: FileOutputStream) = IO(fos.close)

  private[this] def openFIS(f: File) = IO(new FileInputStream(f))

  private[this] def openFOS(f: File, a: Boolean) = IO(new FileOutputStream(f, a))

  private[this] def writeOrAppendFile(b: ByteString, f: File, a: Boolean) =
    openFOS(f, a).bracket(closeFOS)(sPutStr(b, _))
}
