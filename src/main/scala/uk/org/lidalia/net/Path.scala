package uk.org.lidalia
package net

import uk.org.lidalia.scalalang.{ConcretePercentEncodedStringFactory, EncodedString, EncodedStringFactory}
import uk.org.lidalia.net.UriConstants.pchar

import scala.collection.immutable

object Path {

  val empty = new RelativePath(List())
  val root = new AbsolutePath(List(Segment.empty))

  def apply(): Path = empty

  def apply(path: String): Path = {

    def splitPath(p: String) = {
      p.split("/", Int.MaxValue).map(Segment(_)).toList
    }

    def toElements(relPath: String): List[Segment] = {
      if (relPath.isEmpty) List()
      else splitPath(relPath)
    }

    if (path.startsWith("/")) new AbsolutePath(splitPath(path.substring(1)))
    else new RelativePath(toElements(path))
  }
}

abstract class Path private[net] (private [net] val pathElements: immutable.Seq[Segment])
    extends immutable.Seq[Segment] with UriReference {

  private [net] def append(path: Path): Path

  final def resolve(path: Path): Path = path.resolveTo(this)

  private [net] def resolveTo(path: Path): Path

  private [net] def canonicalPath: Path

  private [net] def canonicalSegs: List[Segment] = {
    val temp = pathElements.foldLeft(List[Segment]()) { (acc, elem) =>
      if (elem == Segment.current) acc
      else if (elem == Segment.previous && acc.nonEmpty) acc.init
      else if (elem == Segment.previous && acc.isEmpty) acc
      else acc :+ elem
    }
    if (pathElements.lastOption.exists(_.isSynthetic))
      temp :+ Segment.empty
    else
      temp
  }

  override def length = pathElements.length

  override def apply(idx: Int) = pathElements.apply(idx)

  override def iterator = pathElements.iterator

  val isAbsolute: Boolean

  override private[net] def resolveTo(uri: Uri): Uri = Uri(
    uri.scheme,
    HierarchicalPart(
      uri.hierarchicalPart.authority,
      uri.hierarchicalPart.path.resolve(this)
    ),
    None,
    None
  )

  override lazy val hashCode = toString.hashCode
}

object AbsolutePath {

  def apply(elements: Segment*): Path = apply(elements.toList)

  def apply(elements: immutable.Seq[Segment]): Path = new AbsolutePath(elements)

}

class AbsolutePath private[net] (pathElements: immutable.Seq[Segment])
  extends Path(pathElements) {

  override private [net] def resolveTo(path: Path) = this

  override private [net] def canonicalPath: AbsolutePath = {
    val segs: List[Segment] = canonicalSegs
    if (segs.isEmpty) Path.root else new AbsolutePath(segs)
  }

  override val isAbsolute = true

  override def equals(other: Any): Boolean = other match {
    case that: AbsolutePath =>
      pathElements == that.pathElements
    case _ => false
  }

  override lazy val toString = "/"+pathElements.mkString("/")

  override private [net] def append(path: Path): AbsolutePath = new AbsolutePath(pathElements.init ++ path.pathElements)
}

object RelativePath {

  def apply(elements: Segment*): Path = apply(elements.toList)

  def apply(elements: immutable.Seq[Segment]): Path = new RelativePath(elements)

}

class RelativePath private[net] (pathElements: immutable.Seq[Segment])
  extends Path(pathElements) {

  override private [net] def resolveTo(path: Path) = {
    if (this.isEmpty) path
    else {
      path.append(this).canonicalPath
    }
  }

  override private [net] def canonicalPath: RelativePath = {
    val segs: List[Segment] = canonicalSegs
    new RelativePath(segs)
  }

  override val isAbsolute = false

  override def equals(other: Any): Boolean = other match {
    case that: RelativePath =>
      pathElements == that.pathElements
    case _ => false
  }

  override lazy val toString = pathElements.mkString("/")

  override private [net] def append(path: Path): RelativePath = new RelativePath(pathElements.init ++ path.pathElements)
}
