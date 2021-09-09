package com.estebanmarin
package zioscala3

import zio.*
import zio.console.*
object Main extends App:
  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    (for
      _ <- console.putStrLn("─" * 100)
      _ <- console.putStrLn("What's your name")
      name <- console.getStrLn
      _ <- console.putStrLn(s"Hello $name")
      _ <- console.putStrLn("─" * 100)
    yield ()).exitCode
