package bencode

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

class EncodeSuite extends FunSuite with Checkers
                                   with Matchers {
  test("can encode integer") {
    check((n: Int) =>
      encode(BInt(n)) == s"i${n}e"
    )
  }

  test("can encode list") {
    decodeEqEncode("le", "llee", "llleee", "lli42e3:fooe3:bazi100ee", "li42ee",
      "l4:spame", "li42e4:spame", "lli42eel4:spamee")
  }

  test("can encode dictionary") {
    decodeEqEncode("de", "d3:fooi42ee", "d3:bard2:zoi42eee",
      "d3:fooi42e6:nestedd12:foobarfoobari10eee")
  }

  test("can encode string") {
    check((s: String) =>
      encode(BStr(s)) == s"${s.length}:${s}"
    )
  }

  private def decodeEqEncode(values: String*): Unit = {
    values.foreach(value => {
      val Right(r) = decode(value)
      assert(encode(r) == value)
    })
  }
}
