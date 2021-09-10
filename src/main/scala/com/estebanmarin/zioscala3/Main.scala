package com.estebanmarin
package zioscala3

// import zio.*
import ourzio.*

object Main extends scala.App:
  lazy val program =
    for
      _ <- console.putStrLn("─" * 100)
      _ <- console.putStrLn("What's your name")
      name: String <- ZIO.succeed("Esteban")
      _ <- ZIO.effect(throw RuntimeException("Boom"))
      _ <- console.putStrLn(s"Hello $name")
      _ <- console.putStrLn("─" * 100)
    yield ()

  Runtime.default.unsafeRunSync(program)

// override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
//   program.exitCode
