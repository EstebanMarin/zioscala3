package com.estebanmarin
package zioscala3

import scala.reflect.ClassTag
import zio.ZLayer
import com.estebanmarin.zioscala3.Runtime.default

final class ZIO[-R, +E, +A](val run: R => Either[E, A]):
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO(r => run(r).fold(ZIO.fail, azb).run(r))

  def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    for
      a <- this
      b <- that
    yield a -> b

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO(r => run(r).map(ab))

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO(r => run(r).fold(h, ZIO.succeed).run(r))

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  //This implies the ZEnv Environment
  def provideCustomLayer[R1 <: Has[?]](r1: => R1)(using ZEnv & R1 => R): ZIO[ZEnv, E, A] =
    provideSome[ZEnv](_.union(r1).asInstanceOf[R])

  def provideLayer[R1, E1 >: E](layer: ZLayer[R1, E1, R]): ZIO[R1, E1, A] =
    layer.zio.flatMap(r => provide(r))

  def provideCustom[R1: ClassTag](r1: R1)(using ZEnv & Has[R1] => R): ZIO[ZEnv, E, A] =
    provideCustomLayer(Has(r1))

  // def provideSome[R0](f: R0 => R): ZIO[R0, E, A] =
  //   ZIO.accessM(r0 => provide(f(r0)))
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
  type Console = Has[Console.Service]

  object Console:
    trait Service:
      def putStrLn(line: => String): ZIO[Any, Nothing, Unit]
      def getStrLn: ZIO[Any, Nothing, String]

    lazy val live: ZLayer[Any, Nothing, Console] =
      ZLayer.succed(make)

    lazy val make: Service =
      new:
        def putStrLn(line: => String) =
          ZIO.succeed(println(line))
        lazy val getStrLn =
          ZIO.succeed(scala.io.StdIn.readLine)

  def putStrLn(line: => String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM(_.get.putStrLn(line))

  def getStrLn: ZIO[Console, Nothing, String] =
    ZIO.accessM(_.get.getStrLn)

object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.provideLayer(ZEnv.live).run(())

type ZEnv = console.Console
object ZEnv:
  lazy val live: ZLayer[Any, Nothing, ZEnv] =
    console.Console.live

final class Has[A] private (private val map: Map[String, Any])
object Has:
  def apply[A](a: A)(using tag: ClassTag[A]): Has[A] =
    new Has(Map(tag.toString -> a))

  extension [A <: Has[?]](a: A)
    inline def ++[B <: Has[?]](b: B): A & B =
      union(b)

    infix def union[B <: Has[?]](b: B): A & B =
      new Has(a.map ++ b.map).asInstanceOf[A & B]

    // Do not change order. Current is more type-inference issue
    def get[S](using A => Has[S])(using tag: ClassTag[S]): S =
      a.map(tag.toString).asInstanceOf[S]

final class ZLayer[-R, +E, +A](val zio: ZIO[R, E, A]):
  inline def flatMap[R1 <: R, E1 >: E, B](azb: A => ZLayer[R1, E1, B]): ZLayer[R1, E1, B] =
    ZLayer(this.zio.flatMap(a => azb(a).zio))
  inline def zip[R1 <: R, E1 >: E, B](that: ZLayer[R1, E1, B]): ZLayer[R1, E1, (A, B)] =
    ZLayer(this.zio.zip(that.zio))
  inline def map[B](ab: A => B): ZLayer[R, E, B] =
    ZLayer(this.zio.map(ab))
  inline def provideSome[R0](f: R0 => R): ZLayer[R0, E, A] =
    ZLayer(this.zio.provideSome(f))
  inline def provide(r: => R): ZLayer[Any, E, A] =
    ZLayer(this.zio.provide(r))

object ZLayer:
  def succed[A: ClassTag](
      a: => A
    ): ZLayer[Any, Nothing, Has[A]] =
    ZLayer(ZIO.succeed(Has(a)))

  def fromService[R <: Has[S], S: ClassTag, A: ClassTag](
      f: S => A
    ): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction(r => Has(f(r.get[S]))))

  def fromServices[R <: Has[S1] & Has[S2], S1: ClassTag, S2: ClassTag, A: ClassTag](
      f: (S1, S2) => A
    ): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction(r => Has(f(r.get[S1], r.get[S2]))))
