package uk.org.lidalia
package net2

import uk.org.lidalia.lang.RichObject

object Socket {
  def apply(ipAddress: IpAddress, port: Port) = new Socket(ipAddress, port)
}

class Socket private(
  @Identity val ipAddress: IpAddress,
  @Identity val port: Port
) extends RichObject {
  override def toString = s"${ipAddress.toUriString}:$port"
}
