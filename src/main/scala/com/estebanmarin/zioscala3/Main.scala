package com.estebanmarin
package zioscala3

import com.estebanmarin.zioscala3.businessLogic.BusinessLogic

trait Google:
  def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZIO[Google, Nothing, Service] =
      ZIO.fromFunction(make)

    def make(google: Google): Service =
      new:
        override def doesGoogleHaveEvenAmountOfPicturesOf(
            topic: String
          ): ZIO[Any, Nothing, Boolean] =
          google.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHaveEvenAmountOfPicturesOf(topic))

end businessLogic

object GoogleImp:
  lazy val live: ZIO[Any, Nothing, Google] =
    ZIO.succeed(make)
  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cats" then 1337 else 1338)

object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =
    for
      google <- GoogleImp.live
      businessLogicMake <- businessLogic.BusinessLogic.live.provide(google)
    yield businessLogicMake

  lazy val make: businessLogic.BusinessLogic.Service =
    val g = GoogleImp.make
    val bl = businessLogic.BusinessLogic.make(g)
    bl

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program)
  lazy val program =
    for
      bl <- DependecyGraph.live
      p <- makeProgram.provideCustom(bl)
    yield p

  lazy val makeProgram =
    for
      env <- ZIO.environment[console.Console & businessLogic.BusinessLogic]
      _ <- console.putStrLn("-" * 50)
      cats <- businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("cats")
      _ <- console.putStrLn(cats.toString)
      dogs <- businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("dogs")
      _ <- console.putStrLn(dogs.toString)
      _ <- console.putStrLn("-" * 50)
    yield ()
