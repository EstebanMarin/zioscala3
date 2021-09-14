package com.estebanmarin
package zioscala3

import scala.reflect.ClassTag

final class ZIO[-R, +E, +A](val run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO(r => run(r).fold(ZIO.fail, azb).run(r))

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO(r => run(r).map(ab))

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO(r => run(r).fold(h, ZIO.succeed).run(r))

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  def provideSome[R0](f: R0 => R): ZIO[R0, E, A] =
    for
      r0 <- ZIO.environment[R0]
      a <- provide(f(r0))
    yield a

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

  inline def accessM[R]: AccessMPartiallyApplied[R] =
    AccessMPartiallyApplied()

  inline def read[R]: ZIO[R, Nothing, R] =
    identity

  def identity[R]: ZIO[R, Nothing, R] =
    ZIO.fromFunction(Predef.identity)

final class AccessPartiallyApplied[R]():
  def apply[A](f: R => A): ZIO[R, Nothing, A] =
    ZIO.environment[R].map(f)

final class AccessMPartiallyApplied[R]():
  def apply[A](f: R => ZIO[R, Nothing, A]): ZIO[R, Nothing, A] =
    ZIO.environment.flatMap(f)

object console:
  trait Console:
    def putStrLn(line: => String): ZIO[Any, Nothing, Unit]
    def getStrLn: ZIO[Any, Nothing, String]

  object Console:
    lazy val live: ZIO[Any, Nothing, Console] =
      ZIO.succeed(make)
    lazy val make: Console =
      new:
        def putStrLn(line: => String) =
          ZIO.succeed(println(line))
        lazy val getStrLn =
          ZIO.succeed(scala.io.StdIn.readLine)

  def putStrLn(line: => String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM(_.putStrLn(line))

  def getStrLn: ZIO[Console, Nothing, String] =
    ZIO.accessM(_.getStrLn)

object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[Has[ZEnv], E, A]): Either[E, A] =
      zio.run(Has(console.Console.make))

type ZEnv = console.Console

final class Has[A] private (private val map: Map[String, Any])
object Has:
  def apply[A](a: A)(using tag: ClassTag[A]): Has[A] =
    new Has(Map(tag.toString -> a))

  extension [A <: Has[?]](a: A)
    inline def ++[B <: Has[?]](b: B): A & B =
      union(b)

    infix def union[B <: Has[?]](b: B): A & B =
      new Has(a.map ++ b.map).asInstanceOf[A & B]

    def get[S](using tag: ClassTag[S], view: A => Has[S]): S =
      a.map(tag.toString).asInstanceOf[S]
