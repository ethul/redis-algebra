package redis

import scalaz.{Coproduct, Free}

package object algebra {
  type Seconds = Long

  type Milliseconds = Long

  type Glob = String

  type C0[A] = Coproduct[ConnectionAlgebra, HashAlgebra, A]

  type C1[A] = Coproduct[KeyAlgebra, C0, A]

  type C2[A] = Coproduct[ListAlgebra, C1, A]

  type C3[A] = Coproduct[ScriptAlgebra, C2, A]

  type C4[A] = Coproduct[SetAlgebra, C3, A]

  type C5[A] = Coproduct[StringAlgebra, C4, A]

  type C6[A] = Coproduct[ZSetAlgebra, C5, A]

  type RedisAlgebra[A] = C6[A]

  type RedisAlgebraFree[A] = Free[RedisAlgebra, A]

  type R[A] = RedisAlgebra[A]

  type F[A] = RedisAlgebraFree[A]
}
