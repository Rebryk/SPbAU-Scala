package ru.spbau.mit.wiki

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.text
import org.slf4j.{Logger, LoggerFactory}


object ConstantWikiParser extends WikiParser {
  val logger: Logger = LoggerFactory.getLogger("ConstantWikiParser")

  def loadVersion(url: String, title: String): String = {
    try {
      Thread.sleep(50)
      val version = JsoupBrowser().get(s"$url?title=$title&action=history") >> text(".mw-changeslist-date")
      logger.debug(s"Loaded version of $title")
      version
    } catch {
      case e: Exception =>
        logger.error(s"Failed to load version of $title: $e")
        ""
    }
  }

  def loadPageNode(name: String, url: String, title: String): TreeNode = PageNode(name, title, loadVersion(url, title))

  override def loadContent(url: String): TreeNode = SectionNode("root", List(
    SectionNode(s"1 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Матанализ", url, "Мат.Анализ_1MIT_осень2017"),
        loadPageNode("Основы ДМ и МЛ", url, "Основы_ДМ_и_МЛ_1MIT_осень2017"),
        loadPageNode("Алгебра", url, "Алгебра_1MIT_осень2017"),
        loadPageNode("Алгебра (физики)", url, "Алгебра_phys_1_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("Алгоритмы", url, "Алгоритмы_1MIT_осень2017"),
        loadPageNode("Парадигмы и ЯП", url, "Парадигмы_1MIT_осень2017"),
      ))
    )),
    SectionNode(s"2 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Матанализ", url, "Мат_анализ_2MIT_осень_2017"),
        loadPageNode("Дискретка", url, "Дискретная_математика_2MIT_осень_2017"),
        loadPageNode("Алгебра", url, "Алгебра_2MIT_осень_2017"),
        loadPageNode("Алгебра (физики)", url, "Алгебра_phys_2_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("Java", url, "Java_2MIT_осень_2017"),
        loadPageNode("Алгоритмы", url, "Алгоритмы_2MIT_осень_2017"),
        loadPageNode("ФП", url, "ФП_2MIT_осень_2017"),
        loadPageNode("Архитектура ЭВМ", url, "Архитектура_ЭВМ_2MIT_осень_2017"),
        loadPageNode("Проект Java", url, "Проект_Java_2MIT_осень_2017")
      ))
    )),
    SectionNode(s"3 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Алгоритмы для NP", url, "Алгоритмы_для_NP_трудных_задач_осень_2017"),
        loadPageNode("Диффуры", url, "Дифференциальные_уравнения_3MIT_осень_2017"),
        loadPageNode("Методы оптимизации", url, "Методы_оптимизации_3MIT_осень_2017"),
        loadPageNode("Матстат", url, "Мат_статистика_3MIT_осень_2017"),
        loadPageNode("Формальные языки", url, "Формальные_языки_3MIT_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("Алгоритмы", url, "Алгоритмы_3_3MIT_осень_2017"),
        loadPageNode("Software Engineering", url, "Software_Engineering_3MIT_осень_2017"),
        loadPageNode("БД (Барашев)", url, "Базы_данных_(Барашев)_осень_2017"),
        loadPageNode("Языки для JVM", url, "Альтернативные_языки_для_JVM_осень_2017"),
        loadPageNode("Ядро Linux", url, "Linux_kernel_3MIT_осень_2017"),
        loadPageNode("ROS", url, "Программирование_в_ROS_осень_2017")
      ))
    )),
    SectionNode(s"4 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Вычгеома", url, "Вычислительная_геометрия_осень_2017"),
        loadPageNode("Алгоритмы для NP", url, "Алгоритмы_для_NP_трудных_задач_осень_2017"),
        loadPageNode("Криптография", url, "Криптографические_протоколы_4MIT_осень_2017")
      )),
      SectionNode(s"Машинное обучение 📂", List(
        loadPageNode("Глубинное обучение", url, "Deep_learning_осень_2017"),
        loadPageNode("Машинное обучение-2", url, "Машинное_обучение_2_осень_2017"),
        loadPageNode("Информационный поиск", url, "Информационный_поиск_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("Языки для JVM", url, "Альтернативные_языки_для_JVM_осень_2017"),
        loadPageNode("Компьютерная графика", url, "Компьютерная_графика_осень_2017"),
        loadPageNode("БД (Чернышев)", url, "Базы_данных_(Чернышев)_осень_2017"),
        loadPageNode("ROS", url, "Программирование_в_ROS_осень_2017"),
        loadPageNode("VM", url, "VM_осень_2017")
      )),
      SectionNode(s"Функциональное 📂", List(
        loadPageNode("Зависимые типы", url, "Программирование_с_зависимыми_типами_осень_2017"),
        loadPageNode("Лог. и рел. прогр.", url, "Логическое_и_реляционное_программирование_4MIT_осень_2017"),
        loadPageNode("Метавычисления", url, "Метавычисления_4MIT_осень_2017")
      )),
      SectionNode(s"Другое 📂", List(
        loadPageNode("Граф. интерфейсы", url, "Графические_интерфейсы_осень_2017")
      ))
    )),
    SectionNode(s"5 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Комб. и графы", url, "Комбинаторика_и_теория_графов_5SE_осень_2017"),
        loadPageNode("Алгоритмы", url, "Алгоритмы_и_структуры_данных_5SE_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("БД (Барашев)", url, "Базы_данных_(Барашев)_осень_2017"),
        loadPageNode("БД (Чернышев)", url, "Базы_данных_(Чернышев)_осень_2017"),
        loadPageNode("Unix", url, "Unix_и_скриптовые_языки_5SE_осень_2017"),
        loadPageNode("ОС", url, "OS_5SE_осень_2017"),
        loadPageNode("C++", url, "CPP_5SE_осень_2017"),
        loadPageNode("Основы ПИ", url, "Основы_программной_инженерии_5SE_осень_2017")
      )),
      SectionNode(s"Функциональное 📂", List(
        loadPageNode("ФП", url, "ФП_5SE_осень_2017")
      ))
    )),
    SectionNode(s"6 курс 📂", List(
      SectionNode(s"Математика 📂", List(
        loadPageNode("Вычгеома ", url, "Вычислительная_геометрия_осень_2017")
      )),
      SectionNode(s"Машинное обучение 📂", List(
        loadPageNode("Глубинное обучение", url, "Deep_learning_осень_2017"),
        loadPageNode("Машинное обучение-2", url, "Машинное_обучение_2_осень_2017"),
        loadPageNode("Информационный поиск", url, "Информационный_поиск_осень_2017")
      )),
      SectionNode(s"Программирование 📂", List(
        loadPageNode("Компьютерная графика", url, "Компьютерная_графика_осень_2017"),
        loadPageNode("Языки для JVM", url, "Альтернативные_языки_для_JVM_осень_2017"),
        loadPageNode("Параллелька", url, "Параллельные_и_распределенные_вычисления_6SE_осень_2017"),
        loadPageNode("Java-II", url, "Java-II_6SE_осень_2017"),
        // loadPageNode("Базы Данных (Барашев)", url, "Базы_данных_(Барашев)_осень_2017"),
        loadPageNode("БД (Чернышев)", url, "Базы_данных_(Чернышев)_осень_2017"),
        loadPageNode("VM", url, "VM_осень_2017")
      )),
      SectionNode(s"Функциональное 📂", List(
        loadPageNode("Зависимые типы", url, "Программирование_с_зависимыми_типами_осень_2017")
      )),
      SectionNode(s"Другое 📂", List(
        loadPageNode("Граф. интерфейсы", url, "Графические_интерфейсы_осень_2017")
      ))
    ))
  ))
}
