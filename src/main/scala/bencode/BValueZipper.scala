package bencode

case class BValueZipperHistory(left: Vector[BValue], right: Vector[BValue])

/** Explore BValue data structures. */
case class BValueZipper(focus: BValue, history: List[BValueZipperHistory]) {
  /** Replace focus with given value.
    *
    * @param newValue the new value
    */
  def set(newValue: BValue) = BValueZipper(newValue, history)

  /** Append new value to list in focus.
    *
    * @param value the value to append
    */
  def putList(value: BValue) = focus match {
    case BList(xs) =>
      Some(BValueZipper(BList(xs :+ value), history))
    case _ =>
      None
  }

  /** Walk down into list in focus. */
  def downList = focus match {
    case BList(x +: xs) =>
      Some(BValueZipper(x, BValueZipperHistory(Vector(), xs) +: history))
    case _ =>
      None
  }

  /** Insert new element into dictionary in focus.
    */
  def putDict(field: String, value: BValue) = focus match {
    case BDict(map) =>
      Some(BValueZipper(BDict(map + (field -> value)), history))
    case _ =>
      None
  }

  /** Walk down into item by given field for dictionary in focus.
    *
    * @param field the field to walk down into
    */
  def downDict(field: String) = focus match {
    case BDict(map) =>
      for {
        value <- map.get(field)
        newHistory = BValueZipperHistory(Vector(BDict(map)), Vector(BStr(field)))
      } yield (BValueZipper(value, newHistory +: history))
    case _ =>
      None
  }

  /** Walk up from dictionary or list in focus. */
  def up = history match {
    case BValueZipperHistory(BDict(map) +: _, BStr(field) +: _) +: tail =>
      Some(BValueZipper(BDict(map + (field -> focus)), history.tail))
    case BValueZipperHistory(ls, rs) +: tail =>
      Some(BValueZipper(BList(ls.reverse ++ (focus +: rs)), tail))
    case _ =>
      None
  }

  /** Walk to next element inside list in focus. */
  def forward = history match {
    case BValueZipperHistory(ls, r +: rs) +: tail =>
      Some(BValueZipper(r, BValueZipperHistory(focus +: ls, rs) +: tail))
    case _ =>
      None
  }

  /** Walk to previous element inside list in focus. */
  def back = history match {
    case BValueZipperHistory(l +: ls, rs) +: tail =>
      Some(BValueZipper(l, BValueZipperHistory(ls, l +: rs) +: tail))
    case _ =>
      None
  }

  /** Walk up through history until root. */
  def root: BValueZipper = up match {
    case Some(z) =>
      z.root
    case None =>
      this
  }
}
