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
        case (a: ListAlgebra[A]) => ListAlgebra.listAlgebraFunctor.map(a)(f)
        case (a: SetAlgebra[A]) => SetAlgebra.setAlgebraFunctor.map(a)(f)
        case (a: ZSetAlgebra[A]) => ZSetAlgebra.zsetAlgebraFunctor.map(a)(f)
      }
    }
}

object RedisAlgebra extends RedisInstances
