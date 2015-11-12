package uk.org.lidalia
package net2

import uk.org.lidalia.lang.RichObject
import uk.org.lidalia.scalalang.UnsignedByte

object IpV4Address {

  def apply(ipAddressStr: String) = IpV4AddressParser(ipAddressStr)

  def apply(byte1: UnsignedByte,
            byte2: UnsignedByte,
            byte3: UnsignedByte,
            byte4: UnsignedByte) = new IpV4Address(byte1, byte2, byte3, byte4)
}

class IpV4Address private (
  @Identity val byte1: UnsignedByte,
  @Identity val byte2: UnsignedByte,
  @Identity val byte3: UnsignedByte,
  @Identity val byte4: UnsignedByte
) extends RichObject with IpAddressInternal {
  override val toString = s"$byte1.$byte2.$byte3.$byte4"
  override val toUriString = toString
}
