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

  test("can decode list") {
    Bencode.decode("le") shouldEqual (Right(BList(List())))
    Bencode.decode("llee") shouldEqual Right(BList(List(BList(List()))))
    Bencode.decode("llleee") shouldEqual Right(BList(List(BList(List(BList(List()))))))
    // nested lists with arbitrary ints and strings
    check((n: Int, s: String) =>
      Bencode.decode(s"li${n}e${s.length}:${s}e") ==
        Right(
          BList(List(
            BInt(n), BStr(s))))
      &&
      Bencode.decode(s"llli${n}eel${s.length}:${s}eee${s}") ==
        Right(
          BList(List(
            BList(List(
              BList(List(BInt(n))),
              BList(List(BStr(s))))))))
    )
    // ivalid
    Bencode.decode("l").isLeft shouldEqual (true)
    Bencode.decode("lle").isLeft shouldEqual (true)
    Bencode.decode("li42ei42e").isLeft shouldEqual (true)
    Bencode.decode("lli42ei42ee").isLeft shouldEqual (true)
  }

  test("can decode dictionary") {
    // edge case
    Bencode.decode("de") shouldEqual (
      Right(
        BDict(
          Map.empty)))
    check((name: String, value: Int) => {
      // string -> int map
      Bencode.decode(s"d${name.length}:${name}i${value}ee") ==
        Right(
          BDict(
            Map(
              name -> BInt(value))))
      // nested list
      Bencode.decode(s"d${name.length}:${name}li${value}eee") ==
        Right(
          BDict(
            Map(
              name -> BList(List(BInt(value))))))
      // nested dictionary
      Bencode.decode(s"d3:food${name.length}:${name}i${value}eee") ==
        Right(
          BDict(
            Map(
              "foo" -> BDict(Map(name -> BInt(value))))))
    })
    // invalid
    Bencode.decode("d").isLeft shouldEqual (true)
    Bencode.decode("dde").isLeft shouldEqual (true)
    Bencode.decode("ddee").isLeft shouldEqual (true)
    Bencode.decode("d2:fooi42ee").isLeft shouldEqual (true)
    Bencode.decode("d4:fooi42ee").isLeft shouldEqual (true)
    Bencode.decode("di42e3:fooe").isLeft shouldEqual (true)
    Bencode.decode("d3:fooi42e").isLeft shouldEqual (true)
    Bencode.decode("d3:fooe").isLeft shouldEqual (true)
  }

  test("can decode string") {
    // edge case
    Bencode.decode("0:") shouldEqual (Right(BStr("")))
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