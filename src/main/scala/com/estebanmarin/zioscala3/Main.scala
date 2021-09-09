package com.estebanmarin
package zioscala3

import zio.*
import zio.console.*
object Main extends scala.App:
  val trace = s"[${scala.Console.BLUE}Result of the program =>${scala.Console.RESET}]"
  lazy val program =
    for
      _ <- console.putStrLn("─" * 100).debug(trace)
      _ <- console.putStrLn("What's your name").debug(trace)
      // name <- console.getStrLn
      name: String <- ZIO.succeed("Esteban").debug(trace)
      // // name <- ZIO.fail(error = sys.error("boom"))
      // _ <- ZIO.effect(effect = throw java.lang.RuntimeException("Boom"))
      _ <- console.putStrLn(s"Hello $name")
      _ <- console.putStrLn("─" * 100)
    yield ()

  Runtime.default.unsafeRunSync(program.repeatN(n = 0))

// override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
//   program.exitCode
