package bencode

import org.scalatest.{FunSuite, Matchers}

class ZipperSuite extends FunSuite with Matchers {
  test("can walk down and up in list") {
    val ds = decode("llllli42eeeeee").right.get
    val z = BValueZipper(ds, Nil)
    val r1 = for {
      z2 <- z.downList
      z3 <- z2.downList
      z4 <- z3.downList
      z5 <- z4.downList
      z6 <- z5.downList
    } yield (z6)
    r1.get.focus shouldEqual (BInt(42))
    val r2 = for {
      z <- r1
      z2 <- z.up
      z3 <- z2.up
      z4 <- z3.up
      z5 <- z4.up
      z6 <- z5.up
    } yield (z6)
    r2.get shouldEqual (z)
  }

  test("can walk forward and backward in list") {
    val ds = decode("li42eledei100e3:fooe").right.get
    val z = BValueZipper(ds, Nil)
    val r1 = for {
      z2 <- z.downList
      z3 <- z2.forward
      z4 <- z3.forward
      z5 <- z4.forward
      z6 <- z5.forward
    } yield (z6)
    r1.get.focus shouldEqual (BStr("foo"))
    val r2 = for {
      z <- r1
      z2 <- z.back
      z3 <- z2.back
      z4 <- z3.back
      z5 <- z4.back
    } yield (z5)
    r2.get.focus shouldEqual (BInt(42))
  }

  test("can add items to list") {
    val z = BValueZipper(BList(), Nil)
    val r1 = for {
      z2 <- z.putList(BInt(42))
      z3 <- z2.putList(BList())
      z4 <- z3.putList(BDict())
      z5 <- z4.putList(BInt(100))
      z6 <- z5.putList(BStr("foo"))
    } yield (z6)
    r1.get.root.focus shouldEqual (decode("li42eledei100e3:fooe").right.get)
  }

  test("can walk down and up in dictionary") {
    val ds = decode("d3:food3:bard3:zoti42eeee").right.get
    val z = BValueZipper(ds, Nil)
    val r1 = for {
      z2 <- z.downDict("foo")
      z3 <- z2.downDict("bar")
      z4 <- z3.downDict("zot")
    } yield (z4)
    r1.get.focus shouldEqual (BInt(42))
    val r2 = for {
      z <- r1
      z2 <- z.up
      z3 <- z2.up
      z4 <- z3.up
    } yield (z4)
    r2.get shouldEqual (z)
  }

  test("can add elements to dictionary") {
    val z = BValueZipper(BDict(), Nil)
    val r1 = for {
      z2 <- z.putDict("foo", BDict())
      z3 <- z2.downDict("foo")
      z4 <- z3.putDict("bar", BDict())
      z5 <- z4.downDict("bar")
      z6 <- z5.putDict("zot", BInt(42))
    } yield (z6)
    r1.get.root.focus shouldEqual (decode("d3:food3:bard3:zoti42eeee").right.get)
    val r2 = for {
      z2 <- z.putDict("foo", BInt(42))
      z3 <- z2.putDict("bar", BList())
      z4 <- z3.putDict("zot", BStr("hello"))
    } yield (z4)
    r2.get.root.focus shouldEqual (decode("d3:fooi42e3:barle3:zot5:helloe").right.get)
  }
}
