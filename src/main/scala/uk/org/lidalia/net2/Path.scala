package uk.org.lidalia
package net2

import uk.org.lidalia.scalalang.{ConcretePercentEncodedStringFactory, EncodedString, EncodedStringFactory}
import uk.org.lidalia.net2.UriConstants.pchar

import scala.collection.immutable

object Path extends EncodedStringFactory[Path] {

  private val factory = new ConcretePercentEncodedStringFactory(pchar + '/')

  def apply(): Path = apply(Segment.emptySegment)

  def apply(element: Segment, elements: Segment*): Path = apply(element :: elements.toList)

  def apply(elements: immutable.Seq[Segment]): Path = new Path(elements)

  override def apply(path: String): Path = {
    val elements = path.split("/", Int.MaxValue).map(Segment(_)).toList
    apply(elements)
  }

  override def encode(unencoded: String): Path = apply(factory.encode(unencoded).toString)

}

class Path private[net2] (private val pathElements: immutable.Seq[Segment])
    extends immutable.Seq[Segment] with EncodedString[Path] {

  require(pathElements.nonEmpty, "Path must have at least one segment; segment can be empty")

  override def length = pathElements.length

  override def apply(idx: Int) = pathElements.apply(idx)

  override def iterator = pathElements.iterator

  override lazy val decode = pathElements.map(_.decode).mkString("/")

  override lazy val toString = pathElements.mkString("/")

  override val factory = Path

  val isAbsolute = length > 1 && apply(0).isEmpty

  override def equals(other: Any): Boolean = other match {
    case that: Path =>
      pathElements == that.pathElements
    case _ => false
  }

  override def hashCode() = pathElements.hashCode()
}
