package com.estebanmarin
package zioscala3

import com.estebanmarin.zioscala3.businessLogic.BusinessLogic

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
  def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

object businessLogic:
  trait BusinessLogic:
    def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[Any, Nothing, Boolean]

  object BusinessLogic:
    lazy val live: ZIO[Google, Nothing, BusinessLogic] =
      ZIO.fromFunction(make)

    def make(google: Google): BusinessLogic =
      new:
        override def doesGoogleHaveEvenAmountOfPicturesOf(
            topic: String
          ): ZIO[Any, Nothing, Boolean] =
          google.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.doesGoogleHaveEvenAmountOfPicturesOf(topic))

end businessLogic

object GoogleImp:
  lazy val live: ZIO[Any, Nothing, Google] =
    ZIO.succeed(make)
  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cats" then 1337 else 1338)

object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic] =
    for
      google <- GoogleImp.live
      businessLogicMake <- businessLogic.BusinessLogic.live.provide(google)
    yield businessLogicMake

  lazy val make: businessLogic.BusinessLogic =
    val g = GoogleImp.make
    val bl = businessLogic.BusinessLogic.make(g)
    bl

object Main extends scala.App:
  // Runtime.default.unsafeRunSync(makeProgram.provide(DependecyGraph.make))
  Runtime.default.unsafeRunSync(program)
  lazy val program =
    for
      bl <- DependecyGraph.live
      p <- makeProgram.provide(zio.Has(bl) `union` zio.Has(console.Console.make))
    yield p

  lazy val makeProgram =
    for
      env <- ZIO.environment[zio.Has[businessLogic.BusinessLogic] & zio.Has[console.Console]]
      _ <- env.get[console.Console].putStrLn("-" * 50)
      cats <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPicturesOf("cats")
      _ <- env.get[console.Console].putStrLn(cats.toString)
      dogs <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPicturesOf("dogs")
      _ <- env.get[console.Console].putStrLn(dogs.toString)
      _ <- env.get[console.Console].putStrLn("-" * 50)
    yield ()
