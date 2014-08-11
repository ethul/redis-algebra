package redis
package algebra

import scalaz.{ImmutableArray => IA}

trait ByteStringFunctions {
  def bytes(a: String) = IA.make(a.getBytes(`UTF-8`))

  def string(a: ByteString) = new String(a.toArray, `UTF-8`)

  private val `UTF-8` = "UTF-8"
}
