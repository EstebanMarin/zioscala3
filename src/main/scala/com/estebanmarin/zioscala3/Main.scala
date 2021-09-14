package com.estebanmarin
package zioscala3

// import zio.*
// import ourzio.*

// object Main extends scala.App:
//   Runtime.default.unsafeRunSync(program)
//   lazy val program =
//     for
//       _ <- console.putStrLn("─" * 100)
//       _ <- console.putStrLn("What's your name")
//       name: String <- ZIO.succeed("Esteban")
//       // _ <- ZIO
//       // .effect(throw runtimeexception("boom"))
//       // .maperror(_.getmessage)
//       // .catchAll(h = _ => ZIO.succeed(println("Solved the error")))
//       _ <- console.putStrLn(s"Hello $name")
//       _ <- console.putStrLn("─" * 100)
//     yield ()

trait BusinessLogic:
  def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean

trait Google:
  def countPicturesOf(topic: String): Int

object BusinessLogic:
  lazy val live: ZIO[Google, Nothing, BusinessLogic] =
    ZIO.fromFunction(make)

  def make(google: Google): BusinessLogic =
    new:
      override def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean =
        google.countPicturesOf(topic) % 2 == 0

object GoogleImp:
  lazy val live: ZIO[Any, Nothing, Google] =
    ZIO.succeed(make)
  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): Int =
        if topic == "cats" then 1337 else 1338

object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, BusinessLogic] =
    for
      google <- GoogleImp.live
      businessLogicMake <- BusinessLogic.live.provide(google)
    yield businessLogicMake

  // val google = GoogleImp.live.provide(()).apply(())
  // val businessLogic = BusinessLogic.live.provide(google)
  // businessLogic

  lazy val make: BusinessLogic =
    val google: Google = GoogleImp.make
    val businessLogic = BusinessLogic.make(google)
    businessLogic

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program.provide(DependecyGraph.make))
  lazy val program: ZIO[BusinessLogic, Nothing, Unit] =
    for
      businessLogic <- ZIO.environment
      _ <- console.putStrLn("-" * 50)
      cats <- ZIO
        .environment[BusinessLogic]
        .map(_.doesGoogleHaveEvenAmountOfPicturesOf("cats").toString)
      _ <- console.putStrLn(cats)
      dogs <- ZIO
        .environment[BusinessLogic]
        .map(_.doesGoogleHaveEvenAmountOfPicturesOf("dogs").toString)
      _ <- console.putStrLn(dogs)
      dogs <- ZIO.environment[BusinessLogic].map(_.doesGoogleHaveEvenAmountOfPicturesOf("dogs"))
      _ <- console.putStrLn("-" * 50)
    yield ()
