sealed trait BValue;

import scala.util.{Failure, Success, Try, Right, Left}

case class BInt(value: Int) extends BValue;
case class BStr(value: String) extends BValue;
case class BList(value: Seq[BValue]) extends BValue;
case class BDict(value: Map[String, BValue]) extends BValue;

object Bencode {
  def decode(data: String): Either[String, BValue] = data match {
    case d if d.startsWith("i") => decodeInt(data)
    case _ => Left("not implemented")
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
}
