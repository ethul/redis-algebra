package redis
package algebra
package data

import scalaz.NonEmptyList

sealed abstract class Status
case object Ok extends Status
case object Error extends Status
case object Wrongtype extends Status

sealed abstract class Type
case object String extends Type
case object List extends Type
case object Set extends Type
case object ZSet extends Type
case object Hash extends Type

sealed abstract class ObjectSubcommand
final case class Refcount(key: ByteString) extends ObjectSubcommand
final case class Encoding(key: ByteString) extends ObjectSubcommand
final case class Idletime(key: ByteString) extends ObjectSubcommand

sealed abstract class ObjectResult
final case class RefcountResult(value: Long) extends ObjectResult
final case class EncodingResult(value: ByteString) extends ObjectResult
final case class IdletimeResult(value: Long) extends ObjectResult

sealed abstract class LuaResult
final case class LuaNumber(value: Long) extends LuaResult
final case class LuaString(value: ByteString) extends LuaResult
final case class LuaTable(value: Seq[ByteString]) extends LuaResult
final case class LuaStatus(value: Status) extends LuaResult
final case class LuaBoolean(value: Boolean) extends LuaResult

sealed abstract class Master
final case class Host(name: ByteString, port: Int) extends Master
case object Noone extends Master

sealed abstract class SlowlogSubcommand
final case class Get(limit: Option[Int] = None) extends SlowlogSubcommand
case object Len extends SlowlogSubcommand
case object Reset extends SlowlogSubcommand

sealed abstract class SlowlogResult
final case class GetResult(value: Seq[ByteString]) extends SlowlogResult
final case class LenResult(value: Int) extends SlowlogResult
case object ResetResult extends SlowlogResult

sealed abstract class BitOperation
final case class And(dest: ByteString, keys: NonEmptyList[ByteString]) extends BitOperation
final case class Or(dest: ByteString, keys: NonEmptyList[ByteString]) extends BitOperation
final case class Xor(dest: ByteString, keys: NonEmptyList[ByteString]) extends BitOperation
final case class Not(dest: ByteString, key: ByteString) extends BitOperation

sealed abstract class SetOption
case object Nx extends SetOption
case object Xx extends SetOption

sealed abstract class Endpoint
final case class Closed(value: Double) extends Endpoint
final case class Open(value: Double) extends Endpoint
case object -∞ extends Endpoint
case object +∞ extends Endpoint

sealed abstract class Aggregate
case object Sum extends Aggregate
case object Min extends Aggregate
case object Max extends Aggregate

sealed abstract class Order
case object Asc extends Order
case object Desc extends Order

sealed abstract class Position
case object Before extends Position
case object After extends Position

sealed abstract class By
case object Nosort extends By
final case class Pattern(pattern: ByteString) extends By

final case class Limit(offset: Long, count: Long)
