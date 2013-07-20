package redis
package algebra

import scalaz.Free

sealed trait MapInterpreter {
  def run[A](algebra: Free[KeyAlgebra, A], map: Map[String, String]): A =
    algebra.resume.fold({
      case Del(ks, h) =>
        val (b, c) = ks.foldLeft((0, map)) {
          case ((b,c), a) =>
            if (c.contains(a)) (b + 1, c - a) else (b, c)
        }
        run(h(b), c)

      case Dump(k, h) =>
        run(h(map.get(k).map(_.hashCode.toString)), map)

      case Exists(k, h) =>
        run(h(map.contains(k)), map)
    }, a => a)
}

object MapInterpreter extends MapInterpreter
