package bencode

object encode {
  def apply(data: BValue): String = encodeType(data)

  private def encodeType(data: BValue): String = data match {
    case BInt(i) =>
      s"i${i}e"
    case BList(l) =>
      s"l${l.map(encodeType(_)).mkString}e"
    case BDict(d) =>
      s"d${d.map(encodeDictItem(_)).mkString}e"
    case BStr(s) =>
      s"${s.length}:${s}"
  }

  private def encodeDictItem(tuple: (String, BValue)): String = {
    val s = tuple._1
    val v = tuple._2
    s"${s.length}:${s}${encodeType(v)}"
  }
}
