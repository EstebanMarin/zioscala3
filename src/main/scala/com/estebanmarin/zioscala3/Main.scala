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

    lazy val live: ZLayer[google.Google, Nothing, BusinessLogic] =
      ZLayer.fromService(make)

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
  lazy val live: ZLayer[Any, Nothing, google.Google] =
    ZLayer.succed(make)
  lazy val make: google.Google.Service =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cats" then 1337 else 1338)

object controller:
  type Controller = Has[Controller.Service]
  object Controller:
    trait Service:
      def run: ZIO[Any, Nothing, Unit]

    lazy val live: ZLayer[
      businessLogic.BusinessLogic & console.Console,
      Nothing,
      Controller,
    ] = 
      ZLayer.fromServices(make)

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
  lazy val env: ZLayer[Any, Nothing, controller.Controller] =
    GoogleImp.live >>> businessLogic.BusinessLogic.live ++
      console.Console.live >>>
      controller.Controller.live

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program)
  lazy val program =
    // DependecyGraph.live.flatMap(_.get.run)
    // DependecyGraph.live.flatMap(r => controller.run.provide(r))
  controller.run.provideLayer(DependecyGraph.env)