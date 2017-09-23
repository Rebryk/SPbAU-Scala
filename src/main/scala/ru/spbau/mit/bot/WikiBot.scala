package ru.spbau.mit.bot

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.{Callbacks, Commands}
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{AnswerCallbackQuery, EditMessageReplyMarkup, ParseMode, SendMessage}
import info.mukel.telegrambot4s.models._
import ru.spbau.mit.database.WikiActor._
import ru.spbau.mit.wiki._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Success

class UpdateActor(bot: WikiBot) extends Actor {
  override def receive = {
    case _ => bot.update()
  }
}


class WikiBot(val token: String,
              val database: ActorRef,
              val parser: WikiParser) extends TelegramBot with Polling with Commands with Callbacks {
  val SEWIKI_URL = "http://mit.spbau.ru/sewiki/index.php"
  var ERROR_MSG = s"Произошла ошибка 😟"
  val ROOT = "root"

  var root: AtomicReference[TreeNode] = new AtomicReference(null)
  val nodes: mutable.HashMap[Long, mutable.ArrayBuffer[String]] = mutable.HashMap.empty

  // Updates all information and sends notifications to subscribers.
  def update(): Unit = {
    val newRoot = parser.loadContent(SEWIKI_URL)
    traverse(newRoot, root.get() != null)
    root.set(newRoot)
  }

  onCallbackWithTag("open") {
    cbq =>
      addLastNode(cbq.from.id, cbq.data.getOrElse(ROOT))
      answerCallbackQuery(cbq.id)
      refreshMenu(cbq.message.get)
  }

  onCallbackWithTag("back") {
    cbq =>
      removeLastNode(cbq.from.id)
      answerCallbackQuery(cbq.id)
      refreshMenu(cbq.message.get)
  }

  onCallbackWithTag("subscribe") {
    cbq =>
      answerCallbackQuery(cbq.id)

      findNode(cbq.from.id).get.find(cbq.data.get) match {
        case Some(page: PageNode) =>
          database ! Subscribe(cbq.from.id, page.title)
          refreshMenu(cbq.message.get)
        case _ => reply(text = ERROR_MSG)(message = cbq.message.get)
      }
  }

  onCallbackWithTag("unsubscribe") {
    cbq =>
      answerCallbackQuery(cbq.id)

      findNode(cbq.from.id).get.find(cbq.data.get) match {
        case Some(page: PageNode) =>
          database ! Unsubscribe(cbq.from.id, page.title)
          refreshMenu(cbq.message.get)
        case _ => reply(text = ERROR_MSG)(message = cbq.message.get)
      }
  }

  onCommand("/help") {
    implicit msg => {
      reply("Этот бот позволяет следить за обновлениями страниц курсов на sewiki.\n" +
        "Используй команду /settings, чтобы подписаться на интересующие тебя курсы.")
    }
  }

  onCommand("/settings") {
    implicit msg =>
      nodes.update(msg.chat.id, ArrayBuffer(ROOT))
      sendMenu(msg)
  }

  /** Sends a notification to user with the given id.
    *
    * @param id   chatId
    * @param page page that has changed
    */
  private def sendNotification(id: Long, page: PageNode): Unit = {
    request(
      SendMessage(
        chatId = ChatId(id),
        text = s"Страница обновлена!\n\nСтраница: [${page.name}]($SEWIKI_URL?title=${page.title})\nДата изменения: _${page.version}_",
        parseMode = Some(ParseMode.Markdown))
    )
  }

  /** Sends notifications to all subscribers.
    *
    * @param page page that has changed
    */
  private def broadcastNotification(page: PageNode): Unit = {
    implicit val timeout: Timeout = 1.second
    (database ? GetSubscribers(page.title)).onComplete {
      case Success(Subscribers(ids)) => ids.foreach(sendNotification(_, page))
      case _ => /* error, but do nothing */
    }
  }

  /** Updates information about the given page.
    *
    * @param page   page
    * @param notify if page changes, sends notifications to all subscribers
    */
  private def updatePage(page: PageNode, notify: Boolean): Unit = {
    implicit val timeout: Timeout = 1.second
    (database ? GetPage(page.title)).onComplete {
      case Success(Page(version)) =>
        if (notify && version != page.version) {
          broadcastNotification(page)
        }
      case _ => /* error, but do nothing */
    }

    // update page version
    // database ! AddPage(page.title, page.version)
  }

  /** Traverse tree and updates information about all pages.
    *
    * @param node   current node
    * @param notify if page changes, sends notifications to all subscribers
    */
  private def traverse(node: TreeNode, notify: Boolean = false): Unit = {
    node match {
      case SectionNode(_, childNodes) => childNodes.foreach(traverse(_, notify))
      case page: PageNode => updatePage(page, notify)
    }
  }

  /** Sends a menu with updated checkboxes states.
    *
    * @param message message to replace menu there
    */
  private def refreshMenu(message: Message): Unit = {
    findNode(message.chat.id) match {
      case Some(node: SectionNode) => replaceMarkup(message, buildMenu(message.chat.id, node))
      case _ => reply(text = ERROR_MSG)(message = message)
    }
  }

  private def sendMenu(msg: Message): Unit = {
    reply(text = "Подпишись на те курсы, которые тебе интересны!",
      replyMarkup = Some(buildMenu(msg.chat.id, root.get().asInstanceOf[SectionNode])))(msg)
  }

  // MENU

  private def buildMenu(id: Long, node: SectionNode): InlineKeyboardMarkup = {
    var buttons = node.childNodes.map(buildButton(id, _))

    if (node.name != ROOT) {
      val backButton = InlineKeyboardButton.callbackData(s"Назад 🔙", s"back")
      buttons = buttons :+ backButton
    }

    InlineKeyboardMarkup.singleColumn(buttons)
  }

  private def buildButton(id: Long, node: TreeNode): InlineKeyboardButton = {
    implicit val timeout: Timeout = Timeout(1.second)

    node match {
      case SectionNode(name, _) => InlineKeyboardButton.callbackData(name, s"open$name")
      case PageNode(name, title, _) => Await.result(
        database ? GetSubscription(id, title), 1.second) match {
        case Subscription(true) => InlineKeyboardButton.callbackData(s"$name ✅", s"unsubscribe$name")
        case _ => InlineKeyboardButton.callbackData(name, s"subscribe$name")
      }
    }
  }

  // USER STATE

  private def getLastNode(id: Long): String = nodes.getOrElse(id, ArrayBuffer(ROOT)).last

  private def removeLastNode(id: Long): Unit = {
    val path = nodes.getOrElse(id, ArrayBuffer("root"))
    path.remove(path.size - 1)
  }

  private def addLastNode(id: Long, name: String): Unit = nodes.getOrElseUpdate(id, ArrayBuffer.empty) += name

  private def findNode(id: Long): Option[TreeNode] = {
    println(nodes.getOrElse(id, ArrayBuffer(ROOT)))

    var node: Option[TreeNode] = Some(root.get())

    for (name <- nodes.getOrElse(id, ArrayBuffer(ROOT))) {
      if (node.isDefined) {
        node = node.get.find(name) match {
          case child: Some[TreeNode] => child
          case _ => None
        }
      }
    }

    node
  }

  // API REQUESTS

  private def answerCallbackQuery(callbackQueryId: String): Unit = {
    request(AnswerCallbackQuery(callbackQueryId))
  }

  private def replaceMarkup(message: Message, markup: InlineKeyboardMarkup): Unit = {
    request(EditMessageReplyMarkup(Some(message.chat.id), Some(message.messageId), None, Some(markup)))
  }

}
