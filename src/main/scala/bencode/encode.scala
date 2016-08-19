package bencode

/** Encodes BValue to Bencode string.
  *
  * == Example ==
  * {{{
  * scala> encode(BInt(42))
  * => i42e
  *
  * scala> encode(BStr("foo"))
  * => 3:foo
  *
  * scala> encode(BList(BInt(42), BStr("foo")))
  * => li42e3:fooe
  *
  * scala> encode(BDict("foo" -> BInt(42)))
  * => d3:fooi42ee
  * }}}
  */
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
