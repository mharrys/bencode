package bencode

sealed trait BValue;

case class BInt(value: Int) extends BValue {
  override def toString = value.toString
}

case class BStr(value: String) extends BValue {
  override def toString = value
}

case class BList(value: Vector[BValue]) extends BValue {
  override def toString = value.mkString("[", " ", "]")
}

case class BDict(value: Map[String, BValue]) extends BValue {
  override def toString = value.mkString("{", " ", "}")
}

object BList {
  def apply(values: BValue*) = new BList(Vector(values:_*))
}

object BDict {
  def apply(values: (String, BValue)*) = new BDict(Map(values:_*))
}
