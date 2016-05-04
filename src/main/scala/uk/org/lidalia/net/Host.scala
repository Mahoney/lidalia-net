package uk.org.lidalia
package net

import java.net.{Inet4Address, Inet6Address, InetAddress}

import scala.util.Try

object Host {

  val localhost = Host("localhost")

  def apply(hostStr: String): Host = {
    if (hostStr.startsWith("[") && hostStr.endsWith("]")) {
      val literalStr = hostStr.substring(1, hostStr.size - 1)
      if (literalStr.startsWith("v")) {
        IpVFuture(hostStr)
      } else {
        IpV6Address(hostStr)
      }
    } else {
      Try(IpV4Address(hostStr)).getOrElse(RegisteredName(hostStr))
    }
  }
}

sealed trait Host {
  val toUriString: String

  def withPort(port: ?[Port] = None): HostAndPort = HostAndPort(this, port)
}

object IpAddress {
  def apply(inetAddress: InetAddress) = {
    inetAddress match {
      case i: Inet4Address => IpV4Address(i.getHostAddress)
      case i: Inet6Address => IpV6Address(i.getHostAddress)
      case i: InetAddress => IpVFuture(i.getHostAddress)
    }
  }
}

sealed trait IpAddress extends Host
private [net] trait IpAddressInternal extends IpAddress
private [net] trait RegisteredNameInternal extends Host
