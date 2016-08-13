package bencode

sealed trait BValue;

case class BInt(value: Int) extends BValue;
case class BStr(value: String) extends BValue;
case class BList(value: Seq[BValue]) extends BValue;
case class BDict(value: Map[String, BValue]) extends BValue;
