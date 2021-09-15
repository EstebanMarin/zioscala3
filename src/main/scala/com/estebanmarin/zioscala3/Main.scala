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

    lazy val live: ZIO[google.Google, Nothing, BusinessLogic] =
      ZIO.fromFunction { env =>
        val g = env.get[google.Google.Service]
        Has(make(g))
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
  lazy val live: ZIO[Any, Nothing, google.Google] =
    ZIO.succeed(Has(make))
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
      Controller,
    ] = ZIO.fromFunction{ env =>
      val bl = env.get[businessLogic.BusinessLogic.Service]
      val c = env.get[console.Console.Service]
      Has(make(bl, c))
     }

    def make(bl: businessLogic.BusinessLogic.Service, con: console.Console.Service): Service =
      new:
        override lazy val run: ZIO[Any, Nothing, Unit] =
          for
            _ <- con.putStrLn("-" * 50)
            cats <- bl.doesGoogleHaveEvenAmountOfPicturesOf("cats")
            _ <- con.putStrLn(cats.toString)
            dogs <- bl.doesGoogleHaveEvenAmountOfPicturesOf("dogs")
            _ <- con.putStrLn(dogs.toString)
            _ <- con.putStrLn("-" * 50)
          yield ()

  lazy val run: ZIO[Controller, Nothing, Unit] =
    ZIO.accessM(_.get.run)


object DependecyGraph:
  lazy val live: ZIO[Any, Nothing, controller.Controller] =
    for
      g <- GoogleImp.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
      con <- console.Console.live
      c <- controller.Controller.live.provide(bl ++ con)
    yield c

  lazy val make: controller.Controller.Service =
    val g = GoogleImp.make
    val bl = businessLogic.BusinessLogic.make(g)
    val con = console.Console.make
    val c = controller.Controller.make(bl, con)
    c

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program)
  lazy val program =
    // DependecyGraph.live.flatMap(_.get.run)
    // DependecyGraph.live.flatMap(r => controller.run.provide(r))
    controller.run.provide(Has(DependecyGraph.make))