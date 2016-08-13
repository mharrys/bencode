import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

class BencodeSuite extends FunSuite with Checkers with Matchers {

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
}
