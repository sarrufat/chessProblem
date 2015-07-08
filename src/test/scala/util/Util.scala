package util

import java.io.ByteArrayOutputStream

object Util {
  def stdoutToString(thunk: ⇒ Unit) = {
    val sw = new ByteArrayOutputStream
    Console.withOut(sw)(thunk)
    sw.toString
  }
  def stderrToString(thunk: ⇒ Unit) = {
    val sw = new ByteArrayOutputStream
    Console.withErr(sw)(thunk)
    sw.toString
  }
}
