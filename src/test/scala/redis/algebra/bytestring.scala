package redis
package algebra

import scalaz.{CharSet, ImmutableArray => IA}

trait ByteStringFunctions {
  def bytes(a: String) = IA.make(a.getBytes(CharSet.UTF8.value))

  def string(a: ByteString) = new String(a.toArray, CharSet.UTF8.value)
}
