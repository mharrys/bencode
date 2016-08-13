package bencode

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

class BencodeSuite extends FunSuite with Checkers
                                    with Matchers
                                    with GeneratorDrivenPropertyChecks {
  test("can decode integer") {
    check((n: Int) => {
      decode(s"i${n}e") == Right(BInt(n)) &&
      decode(s"${n}e").isLeft &&
      decode(s"i${n}").isLeft
    })
    check((s: String) => {
      decode(s"i${s}e").isLeft
    })
  }

  test("can decode list") {
    decode("le") shouldEqual (Right(BList(List())))
    decode("llee") shouldEqual Right(BList(List(BList(List()))))
    decode("llleee") shouldEqual Right(BList(List(BList(List(BList(List()))))))
    // nested lists with arbitrary ints and strings
    check((n: Int, s: String) =>
      decode(s"li${n}e${s.length}:${s}e") ==
        Right(
          BList(List(
            BInt(n), BStr(s))))
      &&
      decode(s"llli${n}eel${s.length}:${s}eee${s}") ==
        Right(
          BList(List(
            BList(List(
              BList(List(BInt(n))),
              BList(List(BStr(s))))))))
    )
    // ivalid
    decode("l").isLeft shouldEqual (true)
    decode("lle").isLeft shouldEqual (true)
    decode("li42ei42e").isLeft shouldEqual (true)
    decode("lli42ei42ee").isLeft shouldEqual (true)
  }

  test("can decode dictionary") {
    // edge case
    decode("de") shouldEqual (
      Right(
        BDict(
          Map.empty)))
    check((name: String, value: Int) => {
      // string -> int map
      decode(s"d${name.length}:${name}i${value}ee") ==
        Right(
          BDict(
            Map(
              name -> BInt(value))))
      // nested list
      decode(s"d${name.length}:${name}li${value}eee") ==
        Right(
          BDict(
            Map(
              name -> BList(List(BInt(value))))))
      // nested dictionary
      decode(s"d3:food${name.length}:${name}i${value}eee") ==
        Right(
          BDict(
            Map(
              "foo" -> BDict(Map(name -> BInt(value))))))
    })
    // invalid
    decode("d").isLeft shouldEqual (true)
    decode("dde").isLeft shouldEqual (true)
    decode("ddee").isLeft shouldEqual (true)
    decode("d2:fooi42ee").isLeft shouldEqual (true)
    decode("d4:fooi42ee").isLeft shouldEqual (true)
    decode("di42e3:fooe").isLeft shouldEqual (true)
    decode("d3:fooi42e").isLeft shouldEqual (true)
    decode("d3:fooe").isLeft shouldEqual (true)
  }

  test("can decode string") {
    // edge case
    decode("0:") shouldEqual (Right(BStr("")))
    check((s: String) => {
      decode(s"${s.length}:${s}") == Right(BStr(s)) &&
      decode(s"${s.length}:${s}${s}") == Right(BStr(s)) &&
      decode(s"${s.length}${s}").isLeft
    })
    // test only digits or with appended separator (including negative numbers)
    forAll { (n: Int) =>
      whenever (n > 0) {
        decode(s"${n}").isLeft
        decode(s"${n}:").isLeft
      }
    }
    // test strings that are too short
    forAll { (s: String, n: Int) =>
      whenever (s.length > 0 && n > 0) {
        decode(s"${s.length}:${s.drop(n)}").isLeft
      }
    }
    // test different separators
    forAll { (s: String, c: Char) =>
      whenever (c != ":") {
        decode(s"${s.length}${c}${s}").isLeft
      }
    }
  }
}
