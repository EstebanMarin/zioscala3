package com.estebanmarin
package zioscala3

// import zio.*
import ourzio.*

abstract class Animal
final case class Dog(name: String) extends Animal

abstract class AnimalShelter:
  def adopt(name: String): Animal

final class DogShelter extends AnimalShelter:
  override def adopt(name: String): Animal =
    Dog(name = name)

object Main extends scala.App:
  val shelter: AnimalShelter = new DogShelter
  val animal = shelter.adopt("snoopy")

  lazy val program =
    for
      _ <- console.putStrLn("─" * 100)
      _ <- console.putStrLn("What's your name")
      name: String <- ZIO.succeed("Esteban")
      // _ <- ZIO.fail(java.lang.RuntimeException("boom"))
      _ <- console.putStrLn(s"Hello $name")
      _ <- console.putStrLn("─" * 100)
    yield ()

  Runtime.default.unsafeRunSync(program)

// override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
//   program.exitCode
