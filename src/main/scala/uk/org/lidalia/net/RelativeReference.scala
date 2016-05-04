package uk.org.lidalia.net

import uk.org.lidalia._
import uk.org.lidalia.lang.RichObject

object RelativeReference {

  def apply(ref: String): UriReference = {
    UriParser.parseRelativeReference(ref)
  }

  def apply(
             hierarchicalPart: HierarchicalPart,
             query: ?[Query] = None,
             fragment: ?[Fragment] = None
           ) =
    new RelativeReference(
      hierarchicalPart,
      query,
      fragment
    )
}

class RelativeReference private[net] (
                       @Identity val hierarchicalPart: HierarchicalPart,
                       @Identity val query: ?[Query],
                       @Identity val fragment: ?[Fragment])
  extends RichObject
    with Immutable
    with UriReference {

  override final def toString = {
    val baseUri = s"$hierarchicalPart"
    val uriWithQuery = query.map(q => s"$baseUri?$q") getOrElse baseUri
    val uriWithFragment = fragment.map(f => s"$uriWithQuery#$f") getOrElse uriWithQuery
    uriWithFragment
  }

  lazy val path: Path = hierarchicalPart.path
  lazy val pathAndQuery: PathAndQuery = PathAndQuery(path, query)

  override private [net] def resolveTo(uri: Uri): Uri = {
    Uri(
      uri.scheme,
      HierarchicalPart(
        hierarchicalPart.authority.orElse(uri.hierarchicalPart.authority),
        uri.hierarchicalPart.path.resolve(path)
      ),
      query,
      fragment
    )
  }
}
