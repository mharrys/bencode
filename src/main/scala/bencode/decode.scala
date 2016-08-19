package bencode

import scala.util.{Failure, Success, Try, Right, Left}

/** Attempts to decode string decoded in Bencode as Right(BValue) if possible,
  * otherwise Left(String) is returned with a error message.
  *
  * == Example ==
  * {{{
  * scala> decode("i42e")
  * => Right(BInt(42))
  *
  * scala> decode("3:foo")
  * => Right(BStr(foo))
  *
  * scala> decode("li42e3:fooe")
  * => Right(BList(List(BInt(42), BStr(foo))))
  *
  * scala> decode("d3:fooi42ee")
  * => Right(BDict(Map(foo -> BInt(42))))
  * }}}
  */
object decode {
  def apply(data: String): Either[String, BValue] = decodeType(data) match {
    case Right((result, _)) =>
      Right(result)
    case Left(error) =>
      // need to re-wrap since this method use BValue
      Left(error)
  }

  private def decodeType(data: String): Either[String, (BValue, String)] = data match {
    case d if d startsWith "i" =>
      decodeInt(data)
    case d if d startsWith "l" =>
      decodeList(data)
    case d if d startsWith "d" =>
      decodeDict(data)
    case _ =>
      decodeStr(data)
  }

  private def decodeInt(dataInt: String): Either[String, (BInt, String)] = {
    val (numberStr, rest) = dataInt.tail.span(x => x != 'e') // dropping 'i'
    if (rest.startsWith("e")) {
      Try(numberStr.toInt) match {
        case Success(n) =>
          Right((BInt(n), rest.tail))
        case Failure(_) =>
          Left(s"Unable to parse $numberStr as int")
      }
    } else
      Left("Unexpected ending while parsing int")
  }

  private def decodeList(dataList: String): Either[String, (BList, String)] = {
    @annotation.tailrec
    def parse(data: String, acc: Vector[BValue]): Either[String, (BList, String)] = data match {
      case d if d.isEmpty =>
        Left("Unexpected ending while parsing list")
      case d if d startsWith("e") =>
        Right((BList(acc), data.tail))
      case d =>
        decodeType(data) match {
          case Right((item, tail)) =>
            parse(tail, acc :+ item)
          case Left(error) =>
            Left("Unable to parse list item: " + error)
        }
    }
    parse(dataList.tail, Vector.empty)
  }

  private def decodeDict(dataDict: String): Either[String, (BDict, String)] = {
    @annotation.tailrec
    def parse(data: String, acc: Map[String, BValue]): Either[String, (BDict, String)] = data match {
      case d if d.isEmpty =>
        Left("Unexpected ending while parsing dictionary")
      case d if d startsWith("e") =>
        Right((BDict(acc), data.tail))
      case d =>
        val result = for {
          a <- decodeStr(data).right  // field name
          b <- decodeType(a._2).right // item
        } yield (a._1.value, b._1, b._2)
        result match {
          case Right((name, item, tail)) =>
            parse(tail, acc + (name -> item))
          case Left(error) =>
            Left("Unable to parse dictionary element: " + error)
        }
    }
    parse(dataDict.tail, Map.empty)
  }

  private def decodeStr(dataStr: String): Either[String, (BStr, String)] = {
    val dataStrStart = dataStr takeWhile(Character.isDigit(_))
    if (dataStrStart.length > 0) {
      val strLength = dataStrStart.toInt
      val dataStrTail = dataStr drop dataStrStart.length
      val dataStrLength = dataStrTail.length - 1 // -1 accounts for ":"
      if (dataStrTail.startsWith(":") && dataStrLength >= strLength) {
        val (strContent, tail) = dataStrTail.tail.splitAt(strLength)
        Right((BStr(strContent), tail))
      } else
        Left("Unexpected ending while parsing string")
    } else
      Left("String must start with digits")
  }
}
