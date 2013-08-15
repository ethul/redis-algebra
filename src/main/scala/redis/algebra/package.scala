package redis

import scalaz.{Coproduct, Free}

package object algebra {
  type Seconds = Int

  type Milliseconds = Long

  type Glob = String

  type C0[A] = Coproduct[HashAlgebra, KeyAlgebra, A]

  type C1[A] = Coproduct[ListAlgebra, C0, A]

  type C2[A] = Coproduct[SetAlgebra, C1, A]

  type C3[A] = Coproduct[StringAlgebra, C2, A]

  type C4[A] = Coproduct[ZSetAlgebra, C3, A]

  type RedisAlgebra[A] = C4[A]

  type RedisAlgebraFree[A] = Free[RedisAlgebra, A]

  type R[A] = RedisAlgebra[A]

  type F[A] = RedisAlgebraFree[A]
}
