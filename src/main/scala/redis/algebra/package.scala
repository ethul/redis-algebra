package redis

import scala.collection.immutable.Seq

import scalaz.{Coproduct, Free}

package object algebra {
  type Seconds = Long

  type Milliseconds = Long

  type Microseconds = Int

  type ByteString = Seq[Byte]

  type C0[A] = Coproduct[ConnectionAlgebra, HashAlgebra, A]

  type C1[A] = Coproduct[KeyAlgebra, C0, A]

  type C2[A] = Coproduct[ListAlgebra, C1, A]

  type C3[A] = Coproduct[ScriptAlgebra, C2, A]

  type C4[A] = Coproduct[ServerAlgebra, C3, A]

  type C5[A] = Coproduct[SetAlgebra, C4, A]

  type C6[A] = Coproduct[StringAlgebra, C5, A]

  type C7[A] = Coproduct[ZSetAlgebra, C6, A]

  type RedisAlgebra[A] = C7[A]

  type RedisAlgebraFree[A] = Free[RedisAlgebra, A]

  type R[A] = RedisAlgebra[A]

  type F[A] = RedisAlgebraFree[A]

  object connection extends ConnectionInstances with ConnectionFunctions

  object hash extends HashInstances with HashFunctions

  object key extends KeyInstances with KeyFunctions

  object list extends ListInstances with ListFunctions

  object script extends ScriptInstances with ScriptFunctions

  object server extends ServerInstances with ServerFunctions

  object set extends SetInstances with SetFunctions

  object string extends StringInstances with StringFunctions

  object zset extends ZSetInstances with ZSetFunctions

  object all
    extends ConnectionInstances
    with ConnectionFunctions
    with HashInstances
    with HashFunctions
    with KeyInstances
    with KeyFunctions
    with ListInstances
    with ListFunctions
    with ScriptInstances
    with ScriptFunctions
    with ServerInstances
    with ServerFunctions
    with SetInstances
    with SetFunctions
    with StringInstances
    with StringFunctions
    with ZSetInstances
    with ZSetFunctions
}
