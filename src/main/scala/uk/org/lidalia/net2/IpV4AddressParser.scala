package uk.org.lidalia
package net2

import java.util.regex.Pattern

import uk.org.lidalia.scalalang.UnsignedByte

private [net2] object IpV4AddressParser {
  val DEC_OCTET_RGX = "25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9]"
  val IPV4_ADDRESS_PATTERN = Pattern.compile(
    "("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")")

  def apply(ipv4AddressStr: String): IpV4Address = {
    val ipv4AddressMatcher = IpV4AddressParser.IPV4_ADDRESS_PATTERN.matcher(ipv4AddressStr)
    if (ipv4AddressMatcher.matches()) {
      IpV4Address(
        UnsignedByte(ipv4AddressMatcher.group(1)),
        UnsignedByte(ipv4AddressMatcher.group(2)),
        UnsignedByte(ipv4AddressMatcher.group(3)),
        UnsignedByte(ipv4AddressMatcher.group(4)))
    } else {
      throw new IpV4AddressParseException(ipv4AddressStr)
    }
  }
}

class IpV4AddressParseException(message: String) extends RuntimeException(message)
