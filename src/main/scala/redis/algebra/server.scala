package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions}, Free.Return

sealed trait ServerAlgebra[A]

final case class Bgrewriteaof[A](h: Status => A) extends ServerAlgebra[A]

final case class Bgsave[A](h: Status => A) extends ServerAlgebra[A]

final case class Clientgetname[A](h: Option[String] => A) extends ServerAlgebra[A]

final case class Clientkill[A](ip: String, port: Int, h: Status => A) extends ServerAlgebra[A]

final case class Clientlist[A](h: Seq[Map[String, String]] => A) extends ServerAlgebra[A]

final case class Clientsetname[A](name: String, h: Status => A) extends ServerAlgebra[A]

final case class Configget[A](parameter: Glob, h: Seq[String] => A) extends ServerAlgebra[A]

final case class Configresetstat[A](h: Status => A) extends ServerAlgebra[A]

final case class Configrewrite[A](h: Status => A) extends ServerAlgebra[A]

final case class Configset[A](parameter: String, value: String, h: Status => A) extends ServerAlgebra[A]

final case class Dbsize[A](h: Short => A) extends ServerAlgebra[A]

final case class Debugobject[A](key: String, h: Status => A) extends ServerAlgebra[A]

final case class Debugsegfault[A](h: Status => A) extends ServerAlgebra[A]

final case class Flushall[A](h: Status => A) extends ServerAlgebra[A]

final case class Flushdb[A](h: Status => A) extends ServerAlgebra[A]

final case class Info[A](section: Option[String], h: Map[String, Map[String, String]] => A) extends ServerAlgebra[A]

final case class Lastsave[A](h: Seconds => A) extends ServerAlgebra[A]

final case class Monitor[A](h: Stream[String] => A) extends ServerAlgebra[A]

final case class Save[A](h: Status => A) extends ServerAlgebra[A]

final case class Shutdown[A](save: Option[Boolean], h: Status => A) extends ServerAlgebra[A]

final case class Slaveof[A](master: Master, h: Status => A) extends ServerAlgebra[A]

final case class Slowlog[A](subcommand: SlowlogSubcommand, h: SlowlogResult => A) extends ServerAlgebra[A]

final case class Sync[A](a: A) extends ServerAlgebra[A]

final case class Time[A](h: ((Seconds, Microseconds)) => A) extends ServerAlgebra[A]

trait ServerInstances {
  implicit val serverAlgebraFunctor: Functor[ServerAlgebra] =
    new Functor[ServerAlgebra] {
      def map[A, B](a: ServerAlgebra[A])(f: A => B): ServerAlgebra[B] =
        a match {
          case Bgrewriteaof(h) => Bgrewriteaof(x => f(h(x)))
          case Bgsave(h) => Bgsave(x => f(h(x)))
          case Clientgetname(h) => Clientgetname(x => f(h(x)))
          case Clientkill(i, p, h) => Clientkill(i, p, x => f(h(x)))
          case Clientlist(h) => Clientlist(x => f(h(x)))
          case Clientsetname(n, h) => Clientsetname(n, x => f(h(x)))
          case Configget(p, h) => Configget(p, x => f(h(x)))
          case Configresetstat(h) => Configresetstat(x => f(h(x)))
          case Configrewrite(h) => Configrewrite(x => f(h(x)))
          case Configset(p, v, h) => Configset(p, v, x => f(h(x)))
          case Dbsize(h) => Dbsize(x => f(h(x)))
          case Debugobject(k, h) => Debugobject(k, x => f(h(x)))
          case Debugsegfault(h) => Debugsegfault(x => f(h(x)))
          case Flushall(h) => Flushall(x => f(h(x)))
          case Flushdb(h) => Flushdb(x => f(h(x)))
          case Info(s, h) => Info(s, x => f(h(x)))
          case Lastsave(h) => Lastsave(x => f(h(x)))
          case Monitor(h) => Monitor(x => f(h(x)))
          case Save(h) => Save(x => f(h(x)))
          case Shutdown(s, h) => Shutdown(s, x => f(h(x)))
          case Slaveof(m, h) => Slaveof(m, x => f(h(x)))
          case Slowlog(s, h) => Slowlog(s, x => f(h(x)))
          case Sync(a) => Sync(f(a))
          case Time(h) => Time(x => f(h(x)))
        }
    }
}

trait ServerFunctions extends InjectFunctions {
  def bgrewriteaof[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Bgrewriteaof(Return(_)))

  def bgsave[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Bgsave(Return(_)))

  def clientgetname[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Option[String]] =
    inject[F, ServerAlgebra, Option[String]](Clientgetname(Return(_)))

  def clientkill[F[_]: Functor](ip: String, port: Int)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Clientkill(ip, port, Return(_)))

  def clientlist[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Seq[Map[String, String]]] =
    inject[F, ServerAlgebra, Seq[Map[String, String]]](Clientlist(Return(_)))

  def clientsetname[F[_]: Functor](name: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Clientsetname(name, Return(_)))

  def configget[F[_]: Functor](parameter: Glob)(implicit I: Inject[ServerAlgebra, F]): Free[F, Seq[String]] =
    inject[F, ServerAlgebra, Seq[String]](Configget(parameter, Return(_)))

  def configresetstat[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Configresetstat(Return(_)))

  def configrewrite[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Configrewrite(Return(_)))

  def configset[F[_]: Functor](parameter: String, value: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Configset(parameter, value, Return(_)))

  def dbsize[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Short] =
    inject[F, ServerAlgebra, Short](Dbsize(Return(_)))

  def debugobject[F[_]: Functor](key: String)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Debugobject(key, Return(_)))

  def debugsegfault[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Debugsegfault(Return(_)))

  def flushall[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Flushall(Return(_)))

  def flushdb[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Flushdb(Return(_)))

  def info[F[_]: Functor](section: Option[String] = None)(implicit I: Inject[ServerAlgebra, F]): Free[F, Map[String, Map[String, String]]] =
    inject[F, ServerAlgebra, Map[String, Map[String, String]]](Info(section, Return(_)))

  def lastsave[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Seconds] =
    inject[F, ServerAlgebra, Seconds](Lastsave(Return(_)))

  def monitor[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Stream[String]] =
    inject[F, ServerAlgebra, Stream[String]](Monitor(Return(_)))

  def save[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Save(Return(_)))

  def shutdown[F[_]: Functor](save: Option[Boolean] = None)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Shutdown(save, Return(_)))

  def slaveof[F[_]: Functor](master: Master)(implicit I: Inject[ServerAlgebra, F]): Free[F, Status] =
    inject[F, ServerAlgebra, Status](Slaveof(master, Return(_)))

  def slowlog[F[_]: Functor](subcommand: SlowlogSubcommand)(implicit I: Inject[ServerAlgebra, F]): Free[F, SlowlogResult] =
    inject[F, ServerAlgebra, SlowlogResult](Slowlog(subcommand, Return(_)))

  def sync[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, Unit] =
    inject[F, ServerAlgebra, Unit](Sync(Return(())))

  def time[F[_]: Functor](implicit I: Inject[ServerAlgebra, F]): Free[F, (Seconds, Microseconds)] =
    inject[F, ServerAlgebra, (Seconds, Microseconds)](Time(Return(_)))
}

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
