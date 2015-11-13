package uk.org.lidalia
package net

object HostAndPort {
  def apply(hostAndPortStr: String): HostAndPort = HostAndPortParser.parse(hostAndPortStr)

  def apply(host: Host, port: ?[Port] = None): HostAndPort = {
    port.map(HostWithPort(host, _)) getOrElse HostWithoutPort(host)
  }
}

abstract sealed class HostAndPort extends Immutable {
  val host: Host
  val port: ?[Port]

  override def equals(other: Any) = {
    other match {
      case hostAndPort: HostAndPort =>
        hostAndPort.host == host && hostAndPort.port == port
      case _ => false
    }
  }

  override def hashCode = 31 * (31 * host.hashCode) + port.hashCode
}

object HostWithPort {
  def apply(host: Host, port: Port) = new HostWithPort(host, port)
  def apply(host: String, port: String) = new HostWithPort(Host(host), Port(port))
  def apply(host: String, port: Int) = new HostWithPort(Host(host), Port(port))
}

final class HostWithPort private (
  override val host: Host,
  override val port: Some[Port])
  extends HostAndPort
{
  override def toString = s"$host:${port.get}"
}

object HostWithoutPort {
  def apply(host: Host) = new HostWithoutPort(host)
  def apply(host: String) = new HostWithoutPort(Host(host))
}

final class HostWithoutPort private (override val host: Host) extends HostAndPort {
  override val port = None
  override def toString = host.toString
}
