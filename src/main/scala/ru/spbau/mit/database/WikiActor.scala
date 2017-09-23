package ru.spbau.mit.database

import akka.persistence.PersistentActor

import scala.collection.mutable


class WikiActor extends PersistentActor {

  import WikiActor._

  val subscriptions: mutable.HashMap[String, mutable.Set[Long]] = mutable.HashMap.empty
  val versions: mutable.HashMap[String, String] = mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case AddPage(title, version) => versions.update(title, version)
      case Subscribe(id, title) => subscriptions.getOrElseUpdate(title, mutable.Set.empty).add(id)
      case Unsubscribe(id, title) => subscriptions.getOrElseUpdate(title, mutable.Set.empty).remove(id)
    }
  }

  override def receiveRecover: Receive = {
    case event: Event => receiveEvent(event)
  }

  override def receiveCommand: Receive = {
    case event: Event => receiveEvent(event)
    case GetPage(title) => sender ! Page(versions.getOrElse(title, ""))
    case GetSubscription(id, title) => sender ! Subscription(subscriptions.getOrElse(title, mutable.Set.empty).contains(id))
    case GetSubscribers(title) => sender ! Subscribers(subscriptions.getOrElse(title, mutable.Set.empty))
  }

  override def persistenceId: String = "botan-bot-database"
}


object WikiActor {

  // EVENTS

  trait Event

  case class AddPage(title: String, version: String) extends Event

  case class Subscribe(id: Long, title: String) extends Event

  case class Unsubscribe(id: Long, title: String) extends Event

  // QUERIES

  case class GetPage(title: String)

  case class Page(version: String)

  case class GetSubscription(id: Long, title: String)

  case class Subscription(is_active: Boolean)

  case class GetSubscribers(title: String)

  case class Subscribers(ids: mutable.Set[Long])

}