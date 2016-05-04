package uk.org.lidalia
package net

import uk.org.lidalia.lang.RichObject
import uk.org.lidalia.net.UriConstants.split

object PathAndQuery {
  def apply(pathAndQueryStr: String): PathAndQuery = {
    if (pathAndQueryStr.startsWith("?")) {
      PathAndQuery(Path(), Query(pathAndQueryStr.substring(1)))
    } else {
      val pathAndQuery: (String, ?[String]) = split(pathAndQueryStr, "\\?")
      PathAndQuery(Path(pathAndQuery._1), pathAndQuery._2.map(Query(_)))
    }
  }
  def apply(path: Path, query: ?[Query] = None): PathAndQuery = new PathAndQuery(path, query)
}

class PathAndQuery private (
  @Identity val path: Path,
  @Identity val query: ?[Query]
) extends RichObject with UriReference
{
  override def toString = query.map(q => s"$path?$q") getOrElse path.toString

  override private[net] def resolveTo(uri: Uri): Uri = {
    Uri(
      uri.scheme,
      HierarchicalPart(
        uri.hierarchicalPart.authority,
        uri.hierarchicalPart.path.resolve(this.path)
      ),
      this.query,
      None
    )
  }
}
