package com.estebanmarin
package zioscala3

import com.estebanmarin.zioscala3.businessLogic.BusinessLogic

object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

  def countPicturesOf(topic: String): ZIO[Google, Nothing, Int] =
    ZIO.accessM(_.get.countPicturesOf(topic))

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZIO[google.Google, Nothing, Service] =
      ZIO.fromFunction { env =>
        val g = env.get[google.Google.Service]
        make(g)
      }

    def make(gl: google.Google.Service): Service =
      new:
        override def doesGoogleHaveEvenAmountOfPicturesOf(
            topic: String
          ): ZIO[Any, Nothing, Boolean] =
          gl.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHaveEvenAmountOfPicturesOf(topic))

end businessLogic

object GoogleImp:
  lazy val live: ZIO[Any, Nothing, google.Google.Service] =
    ZIO.succeed(make)
  lazy val make: google.Google.Service =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cats" then 1337 else 1338)

object controller:
  type Controller = Has[Controller.Service]
  object Controller:
    trait Service:
      def run: ZIO[Any, Nothing, Unit]

    lazy val live: ZIO[
      businessLogic.BusinessLogic & console.Console,
      Nothing,
      Service,
    ] = ZIO.fromFunction{ env =>
      val bl = env.get[businessLogic.BusinessLogic.Service]
      val c = env.get[console.Console.Service]
      make(bl, c)
     }

    def make(bl: businessLogic.BusinessLogic.Service, c: console.Console.Service): Service =
      new:
        override lazy val run: ZIO[Any, Nothing, Unit] =
          for
            _ <- c.putStrLn("-" * 50)
            cats <- bl.doesGoogleHaveEvenAmountOfPicturesOf("cats")
            _ <- c.putStrLn(cats.toString)
            dogs <- bl.doesGoogleHaveEvenAmountOfPicturesOf("dogs")
            _ <- c.putStrLn(dogs.toString)
            _ <- c.putStrLn("-" * 50)
          yield ()

    lazy val run: ZIO[Controller, Nothing, Unit] =
      ZIO.accessM(_.get.run)


object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =
    for
      google <- GoogleImp.live
      businessLogicMake <- businessLogic.BusinessLogic.live.provide(Has(google))
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
