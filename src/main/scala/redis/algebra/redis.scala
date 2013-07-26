package redis
package algebra

import scalaz.Functor

trait RedisAlgebra[A]

sealed trait RedisInstances {
  implicit def redisAlgebraFunctor: Functor[RedisAlgebra] =
    new Functor[RedisAlgebra] {
      def map[A, B](a: RedisAlgebra[A])(f: A => B): RedisAlgebra[B] = a match {
        case (a: KeyAlgebra[A]) => KeyAlgebra.keyAlgebraFunctor.map(a)(f)
        case (a: StringAlgebra[A]) => StringAlgebra.stringAlgebraFunctor.map(a)(f)
        case (a: HashAlgebra[A]) => HashAlgebra.hashAlgebraFunctor.map(a)(f)
      }
    }
}

object RedisAlgebra extends RedisInstances
