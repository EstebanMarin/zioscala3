package com.estebanmarin
package zioscala3

object ourzio:
  final case class ZIO[A](thunk: () => A):
    def flatMap[B](azb: A => ZIO[B]): ZIO[B] =
      ZIO.succeed {
        val a: A = thunk()
        val zb: ZIO[B] = azb(a)
        val b: B = zb.thunk()
        b
      }

    def map[B](ab: A => B): ZIO[B] =
      ZIO.succeed {
        val a = thunk()
        val b = ab(a)
        b
      }

  end ZIO

  object ZIO:
    def succeed[A](a: => A): ZIO[A] =
      ZIO(() => a)

  object console:
    def putStrLn(line: => String) =
      ZIO.succeed(println(line))
    def getStrLn =
      ZIO.succeed(scala.io.StdIn.readLine)

  object Runtime:
    object default:
      def unsafeRunSync[A](zio: => ZIO[A]) =
        zio.thunk()
