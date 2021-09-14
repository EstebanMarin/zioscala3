package com.estebanmarin
package zioscala3

final class ZIO[R, +E, +A](val run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO { r =>
      val errorOrA = run(r)
      // val zErrorb = errorOrA.fold(fa = e => ZIO.fail(e), fb = a => azb(a))
      val zErrorb = errorOrA match
        case Right(a) => azb(a)
        case Left(e) => ZIO.fail(e)
      val errorOrB = zErrorb.run(r)
      errorOrB
    }

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO { (r: R) =>
      val errorOrA = run(r)
      val errorOrB = errorOrA match
        case Right(a) => Right(ab(a))
        case Left(e) => Left(e)
      errorOrB
    }

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO { (r: R1) =>
      val errorOrA = run(r)
      // val zErrorb = errorOrA.fold(fa = h, fb = ZIO.succeed)
      val zErrorb = errorOrA match
        case Right(a) => ZIO.succeed(a)
        case Left(e) => h(e)
      val errorOrB = zErrorb.run(r)
      errorOrB
    }

  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO { r =>
      val errorOrA = run(r)
      val errorOrB = errorOrA match
        case Right(a) => Right(a)
        case Left(e) => Left(h(e))

      errorOrB
    }

end ZIO

object ZIO:
  def succeed[A](a: => A): ZIO[Any, Nothing, A] =
    ZIO(r => Right(a))

  def fail[E](e: => E): ZIO[Any, E, Nothing] =
    ZIO(r => Left(e))

  def effect[A](a: => A): ZIO[Any, Throwable, A] =
    ZIO { r =>
      try Right(a)
      catch case ex: Throwable => Left(ex)
    }

  def fromFunction[R, A](run: R => A): ZIO[R, Throwable, A] =
    ZIO(r => Right(run(r)))

object console:
  def putStrLn(line: => String) =
    ZIO.succeed(println(line))
  lazy val getStrLn =
    ZIO.succeed(scala.io.StdIn.readLine)

object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.run(())

type ZEnv = Unit
