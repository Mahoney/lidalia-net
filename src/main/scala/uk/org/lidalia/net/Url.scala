package uk.org.lidalia
package net

object Url {
  def apply(
    url: String
  ): Url =
  {
    val uri = Uri(url)
    uri.hierarchicalPart match {
      case hierarchicalPart: HierarchicalPartWithAuthority =>
        new Url(
          uri.scheme,
          hierarchicalPart,
          uri.query,
          uri.fragment
        )
      case _ => throw new IllegalArgumentException(
        s"$url is not a URL - it has no authority"
      )
    }
  }

  def apply(
    scheme: Scheme,
    hierarchicalPart: HierarchicalPartWithAuthority,
    query: ?[Query] = None,
    fragment: ?[Fragment] = None
  ): Url =
    new Url(
      scheme,
      hierarchicalPart,
      query,
      fragment
    )
}

class Url private (
  scheme: Scheme,
  override val hierarchicalPart: HierarchicalPartWithAuthority,
  query: ?[Query] = None,
  fragment: ?[Fragment] = None
  )

  extends Uri(
    scheme,
    hierarchicalPart,
    query,
    fragment
  )
{

  override lazy val absoluteUri: Url = if (fragment.isEmpty) this else Url(scheme, hierarchicalPart, query, None)

  lazy val hostAndPort: HostAndPort = hierarchicalPart.authority.hostAndPort
  lazy val host: Host = hostAndPort.host
  lazy val resolvedPort: Port = hostAndPort.port.orElse(scheme.defaultPort).getOrElse(
    throw new IllegalStateException(s"No port specified and no default port for scheme in $this")
  )

  lazy val baseUrl: Url = Url(
    scheme,
    HierarchicalPartWithAuthority(
      Authority(
        None,
        HostAndPort(
          host,
          resolvedPort
        )
      ),
      Path()
    )
  )
}
