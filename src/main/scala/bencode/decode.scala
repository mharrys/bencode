package bencode

import scala.util.{Failure, Success, Try, Right, Left}

object decode {
  def apply(data: String): Either[String, BValue] = decodeType(data) match {
    case Right((result, _)) =>
      Right(result)
    case Left(error) =>
      // need to re-wrap since this method use BValue
      Left(error)
  }

  private def decodeType(data: String): Either[String, (BValue, String)] = {
    lazy val dataStrStart = data takeWhile(Character.isDigit(_))
    data match {
      case d if d startsWith "i" =>
        decodeInt(data)
      case d if d startsWith "l" =>
        decodeList(data)
      case d if d startsWith "d" =>
        decodeDict(data)
      case d if dataStrStart.length > 0 =>
        decodeStr(data, dataStrStart)
      case _ =>
        Left("Unknown type")
    }
  }

  private def decodeInt(dataInt: String): Either[String, (BInt, String)] = {
    @annotation.tailrec
    def parse(data: String, acc: Seq[Char]): Either[String, (BInt, String)] = data match {
      case d if d.isEmpty =>
        Left("Unexpected ending while parsing int")
      case d if d startsWith("e") =>
        val numberStr = acc.mkString
        Try(numberStr.toInt) match {
          case Success(n) =>
            val tail = data drop 1
            Right((BInt(n), tail))
          case Failure(_) =>
            Left(s"Unable to parse $numberStr as int")
        }
      case d =>
        parse(data drop 1, acc :+ d.head)
    }
    parse(dataInt drop 1, Seq.empty)
  }

  private def decodeList(dataList: String): Either[String, (BList, String)] = {
    @annotation.tailrec
    def parse(data: String, acc: Seq[BValue]): Either[String, (BList, String)] = data match {
      case d if d.isEmpty =>
        Left("Unexpected ending while parsing list")
      case d if d startsWith("e") =>
        val tail = data drop 1
        Right((BList(acc), tail))
      case d =>
        decodeType(data) match {
          case Right((item, tail)) =>
            parse(tail, acc :+ item)
          case Left(error) =>
            Left("Unable to parse list item: " + error)
        }
    }
    parse(dataList drop 1, Seq.empty)
  }

  private def decodeDict(dataDict: String): Either[String, (BDict, String)] = {
    @annotation.tailrec
    def parse(data: String, acc: Map[String, BValue]): Either[String, (BDict, String)] = data match {
      case d if d.isEmpty =>
        Left("Unexpected ending while parsing dictionary")
      case d if d startsWith("e") =>
        val tail = data drop 1
        Right((BDict(acc), tail))
      case d =>
        decodeType(data) match {
          case Right((BStr(name), t1)) =>
            decodeType(t1) match {
              case Right((item, t2)) =>
                parse(t2, acc + (name -> item))
              case Left(error) =>
                Left("Unable to parse dictionary item: " + error)
            }
          case _ =>
            Left("Unable to parse dictionary name")
        }
    }
    parse(dataDict drop 1, Map.empty)
  }

  private def decodeStr(dataStr: String, dataStrStart: String): Either[String, (BStr, String)] = {
    val strLength = dataStrStart.toInt
    val dataStrTail = dataStr drop dataStrStart.length
    val dataStrLength = dataStrTail.length - 1 // -1 accounts for ":"
    if (dataStrTail.startsWith(":") && dataStrLength >= strLength) {
      val (strContent, tail) = dataStrTail drop 1 splitAt strLength
      Right((BStr(strContent), tail))
    } else
      Left("Unexpected ending while parsing string")
  }
}
