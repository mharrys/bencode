package bencode

object encode {
  def apply(data: BValue): Either[String, String] = data match {
    case BInt(v) => Right(s"i${v}e")
    case BStr(s) => Right(s"${s.length}:${s}")
    case _ => Left("not implemented")
  }
}
