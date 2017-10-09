package ru.spbau.mit

sealed trait MultiSet[+A] {
  def filter(p: A => Boolean): MultiSet[A]

  def filterNot(p: A => Boolean): MultiSet[A]

  def withFilter(p: A => Boolean): MultiSet[A]

  def map[B](f: A => B): MultiSet[B]

  def flatMap[B](f: A => MultiSet[B]): MultiSet[B]

  def apply[B >: A](e: B): Boolean

  def count[B >: A](e: B): Int

  def find[B >: A](e: B): Option[B]

  def &[B >: A](ms: MultiSet[B]): MultiSet[B]

  def |[B >: A](ms: MultiSet[B]): MultiSet[B]

  def ++[B >: A](ms: MultiSet[B]): MultiSet[B]

  // Duplicate each element k times
  def *(k: Int): MultiSet[A]
}

case object Nil extends MultiSet[Nothing] {
  override def filter(p: (Nothing) => Boolean): MultiSet[Nothing] = Nil

  override def filterNot(p: (Nothing) => Boolean): MultiSet[Nothing] = Nil

  override def withFilter(p: (Nothing) => Boolean): MultiSet[Nothing] = Nil

  override def map[B](f: (Nothing) => B): MultiSet[B] = Nil

  override def flatMap[B](f: (Nothing) => MultiSet[B]): MultiSet[B] = Nil

  override def apply[B >: Nothing](e: B): Boolean = false

  override def count[B >: Nothing](e: B): Int = 0

  override def find[B >: Nothing](e: B): Option[Nothing] = None

  override def &[B >: Nothing](ms: MultiSet[B]): MultiSet[B] = Nil

  override def |[B >: Nothing](ms: MultiSet[B]): MultiSet[B] = ms

  override def ++[B >: Nothing](ms: MultiSet[B]): MultiSet[B] = ms

  override def *(k: Int): MultiSet[Nothing] = Nil
}

case class Element[+A](value: A, cnt: Int = 1, tail: MultiSet[A] = Nil) extends MultiSet[A] {
  override def filter(p: (A) => Boolean): MultiSet[A] = (if (p(value)) Element(value, cnt) else Nil) ++ tail.filter(p)

  override def filterNot(p: (A) => Boolean): MultiSet[A] = filter(x => !p(x))

  override def withFilter(p: (A) => Boolean): MultiSet[A] = (if (p(value)) Element(value, cnt) else Nil) ++ tail.withFilter(p)

  override def map[B](f: (A) => B): MultiSet[B] = Element(f(value), cnt, tail.map(f))

  override def flatMap[B](f: (A) => MultiSet[B]): MultiSet[B] = (f(value) * cnt) | tail.flatMap(f)

  override def apply[B >: A](e: B): Boolean = find(e).isDefined

  override def count[B >: A](e: B): Int = if (value == e) cnt else tail.count(e)

  override def find[B >: A](e: B): Option[B] = if (value == e) Some(e) else tail.find(e)

  override def &[B >: A](ms: MultiSet[B]): MultiSet[B] = (Element(value) * cnt.min(ms.count(value))) ++ (tail & ms)

  override def |[B >: A](ms: MultiSet[B]): MultiSet[B] = (Element(value) * (cnt + ms.count(value))) ++ (tail | ms.filter(_ != value))

  override def ++[B >: A](ms: MultiSet[B]): MultiSet[B] = Element(value, cnt, tail ++ ms)

  override def *(k: Int): MultiSet[A] = if (k > 0) Element(value, cnt * k, tail * k) else Nil
}

object MultiSet {
  def apply[A](es: A*): MultiSet[A] = es.groupBy(identity).foldRight[MultiSet[A]](Nil)((it, ms) => Element(it._1, it._2.length, ms))

  def unapplySeq[A](ms: MultiSet[A]): Option[Seq[A]] = ms match {
    case Nil => None
    case Element(value, cnt, tail) => Some(Seq.fill(cnt)(value) ++ unapplySeq(tail).getOrElse(Seq.empty))
  }
}