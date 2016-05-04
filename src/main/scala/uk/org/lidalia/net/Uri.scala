package uk.org.lidalia
package net

import uk.org.lidalia.lang.RichObject

/**
 * Models a URI as defined in
 * <a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a>.
 */
object Uri {

  def apply(uri: String) = UriParser(uri)

  def apply(
         scheme: Scheme,
         hierarchicalPart: HierarchicalPart,
         query: ?[Query] = None,
         fragment: ?[Fragment] = None
       ) =
    new Uri(
      scheme,
      hierarchicalPart,
      query,
      fragment
    )
}

class Uri private[net] (
  @Identity val scheme: Scheme,
  @Identity val hierarchicalPart: HierarchicalPart,
  @Identity val query: ?[Query],
  @Identity val fragment: ?[Fragment])
  extends RichObject
  with Immutable
  with UriReference {

  override final def toString = {
    val baseUri = s"$scheme:$hierarchicalPart"
    val uriWithQuery = query.map(q => s"$baseUri?$q") getOrElse baseUri
    val uriWithFragment = fragment.map(f => s"$uriWithQuery#$f") getOrElse uriWithQuery
    uriWithFragment
  }

  lazy val path: Path = hierarchicalPart.path
  lazy val pathAndQuery: PathAndQuery = PathAndQuery(path, query)
  lazy val absoluteUri: Uri = fragment.map{_=> Uri(scheme, hierarchicalPart, query, None) }.getOrElse(this)

  def resolve(uriReference: UriReference) = uriReference.resolveTo(this)

  override private [net] def resolveTo(uri: Uri): Uri = this
}
