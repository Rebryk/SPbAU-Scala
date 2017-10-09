package ru.spbau.mit

class MultiSetTest extends org.scalatest.FunSuite {
  val ms = MultiSet(1, 2, 2, 3, 3, 3)

  def isEqual[A](ms1: MultiSet[A], ms2: MultiSet[A]): Boolean = ((ms1 & ms2) == ms1) && ((ms2 & ms1) == ms2)

  test("isEqual") {
    assert(isEqual(MultiSet(), MultiSet()))
    assert(isEqual(MultiSet(1, 2, 3), MultiSet(3, 2, 1)))
    assert(!isEqual(MultiSet(1), MultiSet()))
    assert(!isEqual(MultiSet(1, 2), MultiSet(1, 2, 2)))
  }

  test("empty") {
    assert(MultiSet() == Nil)
  }

  test("filter") {
    assert(isEqual(ms.filter(_ == 1), MultiSet(1)))
    assert(isEqual(ms.filter(_ < 3), MultiSet(1, 2, 2)))
    assert(!isEqual(ms.filter(_ == 1), MultiSet(1, 1)))
    assert(!isEqual(ms.filter(_ > 3), MultiSet(1, 2, 2)))
  }

  test("map") {
    assert(isEqual(ms.map(it => it + 1), MultiSet(2, 3, 3, 4, 4, 4)))
    assert(!isEqual(ms.map(it => it * 2), ms))
  }

  test("flatMap") {
    assert(isEqual(Nil.flatMap(_ => MultiSet(1)), Nil))
    assert(isEqual(ms.flatMap(_ => Nil), Nil))
    assert(isEqual(MultiSet(1, 2, 3).flatMap(_ => MultiSet(4, 4)), MultiSet(4) * 6))
    assert(isEqual(MultiSet(1, 2, 3).flatMap(x => MultiSet(x, x + 1)), MultiSet(1, 2, 2, 3, 3, 4)))
  }

  test("apply") {
    assert(ms(1))
    assert(!ms(4))
  }

  test("count") {
    assert(Nil.count(0) == 0)
    assert(ms.count(0) == 0)
    assert(ms.count(1) == 1)
    assert(ms.count(2) == 2)
    assert(ms.count(3) == 3)
  }

  test("find") {
    assert(Nil.find(0).isEmpty)
    assert(ms.find(2).isDefined)
    assert(ms.find(2).get == 2)
    assert(ms.find(4).isEmpty)
  }

  test("&") {
    assert(isEqual(Nil & Nil, Nil))
    assert(isEqual(ms & Nil, Nil))
    assert(isEqual(ms, ms & ms))
    assert(isEqual(MultiSet(1, 2, 2) & MultiSet(2, 2, 3), MultiSet(2, 2)))
    assert(isEqual(MultiSet(1, 2, 2) & ms, MultiSet(1, 2, 2)))
    assert(isEqual(MultiSet(1, 2) & MultiSet(3, 4), Nil))
  }

  test("|") {
    assert(isEqual(Nil | Nil, Nil))
    assert(isEqual(ms | Nil, ms))
    assert(isEqual(MultiSet(1, 2) | MultiSet(1, 2), MultiSet(1, 1, 2, 2)))
    assert(isEqual(MultiSet(1, 2, 2) | MultiSet(2, 2, 3), MultiSet(1, 2, 2, 2, 2, 3)))
    assert(!isEqual(MultiSet(1, 2) | MultiSet(1, 2), MultiSet(1, 2)))
  }

  test("pattern-matching") {
    assert(MultiSet(1, 2, 3) match {
      case MultiSet(e1, e2, e3) => isEqual(MultiSet(1, 2, 3), MultiSet(e1, e2, e3))
      case _ => false
    })

    assert(MultiSet(1, 2, 3) match {
      case MultiSet(e1, _*) => true
      case _ => false
    })

    assert(Nil match {
      case MultiSet(e1, _*) => false
      case _ => true
    })
  }

  test("for") {
    val elements = for {e <- ms if e % 2 == 1} yield -e
    assert(isEqual(elements, ms.filter(_ % 2 == 1).map(-_)))
  }

}
