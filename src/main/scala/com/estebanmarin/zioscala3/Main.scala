package com.estebanmarin
package zioscala3

// import zio.*
// import ourzio.*

// object Main extends scala.App:
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
  lazy val live: Google => BusinessLogic = google => make(google)
  def make(google: Google): BusinessLogic =
    new:
      override def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): Boolean =
        google.countPicturesOf(topic) % 2 == 0

object GoogleImp:
  lazy val live: Any => Google =
    _ => make
  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): Int =
        if topic == "cats" then 1337 else 1338

object DependecyGraph:
  lazy val live: Any => BusinessLogic = _ =>
    val google: Google = GoogleImp.live.apply(())
    val businessLogic = BusinessLogic.live.apply(google)
    businessLogic

  lazy val make: BusinessLogic =
    val google: Google = GoogleImp.make
    //classical dependency injection
    val businessLogic = BusinessLogic.make(google)
    businessLogic

object Main extends scala.App:
  lazy val businessLogic = DependecyGraph.live.apply(())

  lazy val bl = DependecyGraph.live

  println("-" * 50)
  println(businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("cats"))
  println(businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("dogs"))
  println("=" * 50)
