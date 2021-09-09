package com.estebanmarin
package zioscala3

import zio.*
import zio.console.*
import java.io.IOException
object Main extends scala.App:
  val trace = s"[${scala.Console.BLUE}Result of the program =>${scala.Console.RESET}]"
  val program: ZIO[Console, IOException, Unit] =
    for
      _ <- console.putStrLn("─" * 100).debug(trace)
      _ <- console.putStrLn("What's your name").debug(trace)
      // name <- console.getStrLn
      name <- ZIO.succeed("Esteban").debug(trace)
      // name <- ZIO.fail[IOException](error = )
      _ <- console.putStrLn(s"Hello $name")
      _ <- console.putStrLn("─" * 100)
    yield ()

  Runtime.default.unsafeRun(program.repeatN(n = 0))

// override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
//   program.exitCode
