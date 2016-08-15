package bencode

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

class EncodeSuite extends FunSuite with Checkers
                                   with Matchers {
  test("can encode integer") {
    check((n: Int) =>
      encode(BInt(n)) == Right(s"i${n}e")
    )
  }

  test("can encode string") {
    check((s: String) =>
      encode(BStr(s)) == Right(s"${s.length}:${s}")
    )
  }
}
