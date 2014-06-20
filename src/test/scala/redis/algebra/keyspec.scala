package redis
package algebra

import org.specs2._

import scalaz.{ImmutableArray => IA, NonEmptyList}, NonEmptyList._

import all._

class KeyAlgebraSpec extends Specification with ByteStringFunctions { def is = s2"""
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

  def e1 = interpreter.run(del(nels(keya, keyb, nonexisting)), map) === 2

  def e2 = interpreter.run(dump(keya), map).map(string(_)) must beSome(serialize(string(keya)))

  def e3 = interpreter.run(dump(nonexisting), map) must beNone

  def e4 = interpreter.run(exists(keya), map) === true

  def e5 = interpreter.run(exists(nonexisting), map) === false

  def serialize(k: String) = map(k).hashCode.toString

  val keya = bytes("a")

  val keyb = bytes("b")

  val keyc = bytes("c")

  val nonexisting = bytes("x")

  val map = Map(string(keya) -> "x", string(keyb) -> "y", string(keyc) -> "z")

  val interpreter = Interpreter
}
