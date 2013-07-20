package redis
package algebra

import org.specs2._

import KeyAlgebra._

class KeyAlgebraSpec extends Specification { def is = s2"""
  This is the specification for the KeyAlgebra.

  A Del command issued with some existing keys should
    result in the number of keys deleted                         $e1

  A Dump command issued with an existing key should
    result in a some(serialized value)                           $e2

  A Dump command issued with a nonexisting key should
    result in a none                                             $e3

  An Exists command issued with an existing key should
    result in true                                               $e4

  An Exists command issued with a nonexisting key should
    result in false                                              $e5
  """

  def e1 = interpreter.run(del(keya :: keyb :: nonexisting :: Nil), map) === 2

  def e2 = interpreter.run(dump(keya), map) must beSome(serialize("a"))

  def e3 = interpreter.run(dump(nonexisting), map) must beNone

  def e4 = interpreter.run(exists(keya), map) === true

  def e5 = interpreter.run(exists(nonexisting), map) === false

  def serialize(k: String) = map(k).hashCode.toString

  val keya = "a"

  val keyb = "b"

  val keyc = "c"

  val nonexisting = "x"

  val map = Map(keya -> "x", keyb -> "y", keyc -> "z")

  val interpreter = MapInterpreter
}
