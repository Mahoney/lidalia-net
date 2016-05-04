package uk.org.lidalia
package net

import uk.org.lidalia.scalalang.{ConcretePercentEncodedStringFactory, EncodedString, EncodedStringFactory}
import uk.org.lidalia.net.UriConstants.pchar

import scala.collection.immutable

object Path extends EncodedStringFactory[Path] {

  private val factory = new ConcretePercentEncodedStringFactory(pchar + '/')

  val empty = apply()
  val root = apply(Segment.empty, Segment.empty)

  def apply(): Path = apply(Segment.empty)

  def apply(element: Segment, elements: Segment*): Path = apply(element :: elements.toList)

  def apply(elements: immutable.Seq[Segment]): Path = new Path(elements)

  override def apply(path: String): Path = {
    val elements = path.split("/", Int.MaxValue).map(Segment(_)).toList
    apply(elements)
  }

  override def encode(unencoded: String): Path = apply(factory.encode(unencoded).toString)

}

class Path private[net] (private val pathElements: immutable.Seq[Segment])
    extends immutable.Seq[Segment] with EncodedString[Path] with UriReference {

  require(pathElements.nonEmpty, "Path must have at least one segment; segment can be empty")

  def resolve(path: Path): Path = {
    if (path.isAbsolute) path
    else {
      Path(pathElements.init ++ path.pathElements).absolutePath
    }
  }

  lazy val absolutePath: Path = {
    Path(pathElements.foldLeft(List[Segment]()) { (acc, elem) =>
      if (elem == Segment.current) acc
      else if (elem == Segment.previous) acc.init
      else acc :+ elem
    })
  }

  override def length = pathElements.length

  override def apply(idx: Int) = pathElements.apply(idx)

  override def iterator = pathElements.iterator

  override lazy val decode = pathElements.map(_.decode).mkString("/")

  override lazy val toString = pathElements.mkString("/")

  override val factory = Path

  val isAbsolute = length > 1 && pathElements.head == Segment.empty

  override def equals(other: Any): Boolean = other match {
    case that: Path =>
      pathElements == that.pathElements
    case _ => false
  }

  override def hashCode() = pathElements.hashCode()

  override private[net] def resolveTo(uri: Uri): Uri = Uri(
    uri.scheme,
    HierarchicalPart(
      uri.hierarchicalPart.authority,
      uri.hierarchicalPart.path.resolve(this)
    ),
    None,
    None
  )
}
