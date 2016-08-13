import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

class BencodeSuite extends FunSuite with Checkers
                                    with Matchers
                                    with GeneratorDrivenPropertyChecks {
  test("can decode integer") {
    check((n: Int) => {
      Bencode.decode(s"i${n}e") == Right(BInt(n)) &&
      Bencode.decode(s"${n}e").isLeft &&
      Bencode.decode(s"i${n}").isLeft
    })
    check((s: String) => {
      Bencode.decode(s"i${s}e").isLeft
    })
  }

  test("can decode string") {
    // edge case
    Bencode.decode(s"0:") shouldEqual (Right(BStr("")))
    check((s: String) => {
      Bencode.decode(s"${s.length}:${s}") == Right(BStr(s)) &&
      Bencode.decode(s"${s.length}:${s}${s}") == Right(BStr(s)) &&
      Bencode.decode(s"${s.length}${s}").isLeft
    })
    // test only digits or with appended separator (including negative numbers)
    forAll { (n: Int) =>
      whenever (n > 0) {
        Bencode.decode(s"${n}").isLeft
        Bencode.decode(s"${n}:").isLeft
      }
    }
    // test strings that are too short
    forAll { (s: String, n: Int) =>
      whenever (s.length > 0 && n > 0) {
        Bencode.decode(s"${s.length}:${s.drop(n)}").isLeft
      }
    }
    // test different separators
    forAll { (s: String, c: Char) =>
      whenever (c != ":") {
        Bencode.decode(s"${s.length}${c}${s}").isLeft
      }
    }
  }
}
