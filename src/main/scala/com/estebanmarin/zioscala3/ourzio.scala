package com.estebanmarin
package zioscala3

object ourzio:
  final case class ZIO[+E, A](thunk: () => Either[E, A]):
    def flatMap[E1 >: E, B](azb: A => ZIO[E1, B]): ZIO[E1, B] =
      ZIO { () =>
        val errorOrA = thunk()
        val zErrorb = errorOrA match
          case Right(a) => azb(a)
          case Left(e) => ZIO.fail(e)
        val errorOrB = zErrorb.thunk()
        errorOrB
      }

    def map[B](ab: A => B): ZIO[E, B] =
      ZIO { () =>
        val errorOrA = thunk()
        val errorOrB = errorOrA match
          case Right(a) => Right(ab(a))
          case Left(e) => Left(e)

        errorOrB
      }

  end ZIO

  object ZIO:
    def succeed[A](a: => A): ZIO[Nothing, A] =
      ZIO(() => Right(a))

    def fail[E](e: => E): ZIO[E, Nothing] =
      ZIO(() => Left(e))

  object console:
    def putStrLn(line: => String) =
      ZIO.succeed(println(line))
    def getStrLn =
      ZIO.succeed(scala.io.StdIn.readLine)

  object Runtime:
    object default:
      def unsafeRunSync[E, A](zio: => ZIO[E, A]) =
        zio.thunk()
