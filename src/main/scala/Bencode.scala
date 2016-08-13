sealed trait BValue;

import scala.util.{Failure, Success, Try, Right, Left}

case class BInt(value: Int) extends BValue;
case class BStr(value: String) extends BValue;
case class BList(value: Seq[BValue]) extends BValue;
case class BDict(value: Map[String, BValue]) extends BValue;

object Bencode {
  def decode(data: String): Either[String, BValue] = {
    lazy val dataStrStart = data takeWhile(Character.isDigit(_))
    data match {
      case d if d startsWith "i" =>
        decodeInt(data)
      case d if dataStrStart.length > 0 =>
        decodeStr(data, dataStrStart)
      case _ =>
        Left("not implemented")
    }
  }

  private def decodeInt(dataInt: String): Either[String, BInt] = {
    @annotation.tailrec
    def parse(data: String, acc: Seq[Char]): Either[String, BInt] = data match {
      case d if d.isEmpty => Left("Unexpected ending while parsing int")
      case d if d startsWith("e") =>
        val numberStr = acc.mkString
        Try(numberStr.toInt) match {
          case Success(n) => Right(BInt(n))
          case Failure(_) => Left(s"Unable to parse $numberStr as int")
        }
      case d => parse(data drop 1, acc :+ d.head)
    }
    parse(dataInt drop 1, Seq())
  }

  private def decodeStr(dataStr: String, dataStrStart: String): Either[String, BStr] = {
    val strLength = dataStrStart.toInt
    val dataStrTail = dataStr drop dataStrStart.length
    val dataStrLength = dataStrTail.length - 1 // -1 accounts for ":"
    if (dataStrTail.startsWith(":") && dataStrLength >= strLength) {
      val strContent = dataStrTail drop 1 take strLength
      Right(BStr(strContent))
    } else
      Left("Unexpected ending while parsing string")
  }
}
