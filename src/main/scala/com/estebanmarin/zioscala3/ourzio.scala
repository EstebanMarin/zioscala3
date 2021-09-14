package com.estebanmarin
package zioscala3

final class ZIO[-R, +E, +A](val run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO(r => run(r).fold(ZIO.fail, azb).run(r))

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO(r => run(r).map(ab))

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO(r => run(r).fold(h, ZIO.succeed).run(r))

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO(r => run(r).left.map(h))

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

  def fromFunction[R, A](run: R => A): ZIO[R, Nothing, A] =
    ZIO(r => Right(run(r)))

  inline def environment[R]: ZIO[R, Nothing, R] =
    identity

  inline def access[R]: AccessPartiallyApplied[R] =
    AccessPartiallyApplied()

  final class AccessPartiallyApplied[R]():
    def apply[A](f: R => A): ZIO[R, Nothing, A] =
      environment[R].map(f)

  inline def accessM[R]: AccessMPartiallyApplied[R] =
    AccessMPartiallyApplied()

  final class AccessMPartiallyApplied[R]():
    def apply[A](f: R => ZIO[R, Nothing, A]): ZIO[R, Nothing, A] =
      environment.flatMap(f)

  inline def read[R]: ZIO[R, Nothing, R] =
    identity

  def identity[R]: ZIO[R, Nothing, R] =
    ZIO.fromFunction(Predef.identity)

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
