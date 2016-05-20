package uk.org.lidalia.net

import uk.org.lidalia.scalalang.{PercentEncodedString, PercentEncodedStringFactory}
import uk.org.lidalia.net.UriConstants.pchar

object Segment extends PercentEncodedStringFactory[Segment](pchar) {

  def apply(pathElementStr: String) = new Segment(pathElementStr)

  val empty = Segment("")
  val current = Segment(".")
  val previous = Segment("..")

}

class Segment private (pathStr: String)
    extends PercentEncodedString[Segment](Segment, pathStr) {
  def isSynthetic: Boolean = pathStr == "." || pathStr == ".."
}

