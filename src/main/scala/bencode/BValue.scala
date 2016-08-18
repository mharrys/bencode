package bencode

sealed trait BValue;

case class BInt(value: Int) extends BValue;
case class BStr(value: String) extends BValue;
case class BList(value: Vector[BValue]) extends BValue;
case class BDict(value: Map[String, BValue]) extends BValue;

object BList {
  def apply(values: BValue*) = new BList(Vector(values:_*))
}

object BDict {
  def apply(values: (String, BValue)*) = new BDict(Map(values:_*))
}
