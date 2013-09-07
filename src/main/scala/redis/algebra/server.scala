package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions}, Free.Return

sealed trait ServerAlgebra[A]

final case class Bgrewriteaof[A](a: A) extends ServerAlgebra[A]

final case class Bgsave[A](a: A) extends ServerAlgebra[A]

final case class Clientgetname[A](h: Option[String] => A) extends ServerAlgebra[A]

final case class Clientkill[A](ip: String, port: Int, a: A) extends ServerAlgebra[A]

final case class Clientlist[A](h: Seq[Map[String, String]] => A) extends ServerAlgebra[A]

final case class Clientsetname[A](name: String, a: A) extends ServerAlgebra[A]

final case class Configget[A](parameter: Glob, h: Seq[String] => A) extends ServerAlgebra[A]

final case class Configresetstat[A](a: A) extends ServerAlgebra[A]

final case class Configrewrite[A](a: A) extends ServerAlgebra[A]

final case class Configset[A](parameter: String, value: String, a: A) extends ServerAlgebra[A]

final case class Dbsize[A](h: Short => A) extends ServerAlgebra[A]

final case class Debugobject[A](key: String, a: A) extends ServerAlgebra[A]

final case class Debugsegfault[A](a: A) extends ServerAlgebra[A]

final case class Flushall[A](a: A) extends ServerAlgebra[A]

final case class Flushdb[A](a: A) extends ServerAlgebra[A]

final case class Info[A](section: Option[String], h: Map[String, Map[String, String]] => A) extends ServerAlgebra[A]

final case class Lastsave[A](h: Seconds => A) extends ServerAlgebra[A]

final case class Monitor[A](h: Stream[String] => A) extends ServerAlgebra[A]

final case class Save[A](a: A) extends ServerAlgebra[A]

final case class Shutdown[A](save: Option[Boolean], a: A) extends ServerAlgebra[A]

final case class Slaveof[A](master: ServerTypes#Master, a: A) extends ServerAlgebra[A]

final case class Slowlog[A](subcommand: ServerTypes#SlowlogSubcommand, h: ServerTypes#SlowlogResult => A) extends ServerAlgebra[A]

final case class Sync[A](a: A) extends ServerAlgebra[A]

final case class Time[A](h: ((Seconds, Microseconds)) => A) extends ServerAlgebra[A]

trait ServerInstances {
  implicit val serverAlgebraFunctor: Functor[ServerAlgebra] =
    new Functor[ServerAlgebra] {
      def map[A, B](a: ServerAlgebra[A])(f: A => B): ServerAlgebra[B] =
        a match {
          case Bgrewriteaof(a) => Bgrewriteaof(f(a))
          case Bgsave(a) => Bgsave(f(a))
          case Clientgetname(h) => Clientgetname(x => f(h(x)))
          case Clientkill(i, p, a) => Clientkill(i, p, f(a))
          case Clientlist(h) => Clientlist(x => f(h(x)))
          case Clientsetname(n, a) => Clientsetname(n, f(a))
          case Configget(p, h) => Configget(p, x => f(h(x)))
          case Configresetstat(a) => Configresetstat(f(a))
          case Configrewrite(a) => Configrewrite(f(a))
          case Configset(p, v, a) => Configset(p, v, f(a))
          case Dbsize(h) => Dbsize(x => f(h(x)))
          case Debugobject(k, a) => Debugobject(k, f(a))
          case Debugsegfault(a) => Debugsegfault(f(a))
          case Flushall(a) => Flushall(f(a))
          case Flushdb(a) => Flushdb(f(a))
          case Info(s, h) => Info(s, x => f(h(x)))
          case Lastsave(h) => Lastsave(x => f(h(x)))
          case Monitor(h) => Monitor(x => f(h(x)))
          case Save(a) => Save(f(a))
          case Shutdown(s, a) => Shutdown(s, f(a))
          case Slaveof(m, a) => Slaveof(m, f(a))
          case Slowlog(s, h) => Slowlog(s, x => f(h(x)))
          case Sync(a) => Sync(f(a))
          case Time(h) => Time(x => f(h(x)))
        }
    }
}

trait ServerFunctions extends InjectFunctions {
  def bgrewriteaof[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Bgrewriteaof(Return(())))

  def bgsave[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Bgsave(Return(())))

  def clientgetname[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Option[String]] =
    inject[F, ServerAlgebra, Option[String]](Clientgetname(Return(_)))

  def clientkill[F[_]: Functor](ip: String, port: Int)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Clientkill(ip, port, Return(())))

  def clientlist[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Seq[Map[String, String]]] =
    inject[F, ServerAlgebra, Seq[Map[String, String]]](Clientlist(Return(_)))

  def clientsetname[F[_]: Functor](name: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Clientsetname(name, Return(())))

  def configget[F[_]: Functor](parameter: Glob)(implicit I: Inject[ServerAlgebra, F]): Free[F, Seq[String]] =
    inject[F, ServerAlgebra, Seq[String]](Configget(parameter, Return(_)))

  def configresetstat[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Configresetstat(Return(())))

  def configrewrite[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Configrewrite(Return(())))

  def configset[F[_]: Functor](parameter: String, value: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Configset(parameter, value, Return(())))

  def dbsize[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Short] =
    inject[F, ServerAlgebra, Short](Dbsize(Return(_)))

  def debugobject[F[_]: Functor](key: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Debugobject(key, Return(())))

  def debugsegfault[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Debugsegfault(Return(())))

  def flushall[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Flushall(Return(())))

  def flushdb[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Flushdb(Return(())))

  def info[F[_]: Functor](section: Option[String] = None)(implicit I: Inject[ServerAlgebra, F]): Free[F, Map[String, Map[String, String]]] =
    inject[F, ServerAlgebra, Map[String, Map[String, String]]](Info(section, Return(_)))

  def lastsave[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Seconds] =
    inject[F, ServerAlgebra, Seconds](Lastsave(Return(_)))

  def monitor[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Stream[String]] =
    inject[F, ServerAlgebra, Stream[String]](Monitor(Return(_)))

  def save[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Save(Return(())))

  def shutdown[F[_]: Functor](save: Option[Boolean] = None)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Shutdown(save, Return(())))

  def slaveof[F[_]: Functor](master: ServerTypes#Master)(implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Slaveof(master, Return(())))

  def slowlog[F[_]: Functor](subcommand: ServerTypes#SlowlogSubcommand)(implicit I: Inject[ServerAlgebra, F]): Free[F, ServerTypes#SlowlogResult] =
    inject[F, ServerAlgebra, ServerTypes#SlowlogResult](Slowlog(subcommand, Return(_)))

  def sync[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Sync(Return(())))

  def time[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, (Seconds, Microseconds)] =
    inject[F, ServerAlgebra, (Seconds, Microseconds)](Time(Return(_)))
}

trait ServerTypes {
  sealed trait Master
  case class Host(name: String, port: Int) extends Master
  case object Noone extends Master

  sealed trait SlowlogSubcommand
  case class GetSubcommand(limit: Option[Int] = None) extends SlowlogSubcommand
  case object LenSubcommand extends SlowlogSubcommand
  case object ResetSubcommand extends SlowlogSubcommand

  sealed trait SlowlogResult
  case class GetResult(value: Seq[String]) extends SlowlogResult
  case class LenResult(value: Int) extends SlowlogResult
  case object ResetResult extends SlowlogResult
}
