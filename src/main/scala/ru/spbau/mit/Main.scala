package ru.spbau.mit

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.mit.bot.{UpdateActor, WikiBot}
import ru.spbau.mit.database.WikiActor
import ru.spbau.mit.wiki.ConstantWikiParser

import scala.concurrent.duration._

object Main extends App {
  val token = "427590281:AAHF16-KpipMHEtpf6L1Rkg6KaJS3xK5_Pw"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[WikiActor]))

  private val bot = new WikiBot(token, database, ConstantWikiParser)
  private val updateActor = system.actorOf(Props(classOf[UpdateActor], bot))

  bot.update()
  bot.run()

  import system.dispatcher

  system.scheduler.schedule(
    0.milliseconds,
    30.minutes,
    updateActor,
    "update"
  )
}
