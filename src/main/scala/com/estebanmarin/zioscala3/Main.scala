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

trait Google:
  def countPicturesOf(topic: String): Int

object businessLogic:
  trait BusinessLogic:
    def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean

  object BusinessLogic:
    lazy val live: ZIO[Google, Nothing, BusinessLogic] =
      ZIO.fromFunction(make)

    def make(google: Google): BusinessLogic =
      new:
        override def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean =
          google.countPicturesOf(topic) % 2 == 0

// def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean =

end businessLogic

object GoogleImp:
  lazy val live: ZIO[Any, Nothing, Google] =
    ZIO.succeed(make)
  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): Int =
        if topic == "cats" then 1337 else 1338

object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic] =
    for
      google <- GoogleImp.live
      businessLogicMake <- businessLogic.BusinessLogic.live.provide(google)
    yield businessLogicMake

  lazy val make: businessLogic.BusinessLogic =
    val g: Google = GoogleImp.make
    val bl = businessLogic.BusinessLogic.make(g)
    bl

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program.provide(DependecyGraph.make))
  lazy val program: ZIO[businessLogic.BusinessLogic, Nothing, Unit] =
    for
      _ <- console.putStrLn("-" * 50)
      cats <- ZIO
        .access[businessLogic.BusinessLogic](
          _.doesGoogleHaveEvenAmountOfPicturesOf("cats")
        )
      _ <- console.putStrLn(cats.toString)
      dogs <- ZIO
        .access[businessLogic.BusinessLogic](
          _.doesGoogleHaveEvenAmountOfPicturesOf("dogs")
        )
      _ <- console.putStrLn(dogs.toString)
      _ <- console.putStrLn("-" * 50)
    yield ()
