package com.estebanmarin
package zioscala3

import com.estebanmarin.zioscala3.businessLogic.BusinessLogic

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
      p <- makeProgram.provideCustom(bl)
    yield p

  lazy val makeProgram =
    for
      env <- ZIO.environment[Has[businessLogic.BusinessLogic] & Has[console.Console]]
      _ <- env.get[console.Console].putStrLn("-" * 50)
      cats <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPicturesOf("cats")
      _ <- env.get[console.Console].putStrLn(cats.toString)
      dogs <- env.get[businessLogic.BusinessLogic].doesGoogleHaveEvenAmountOfPicturesOf("dogs")
      _ <- env.get[console.Console].putStrLn(dogs.toString)
      _ <- env.get[console.Console].putStrLn("-" * 50)
    yield ()
