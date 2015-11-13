package uk.org.lidalia.net

object IpV6Address {
  def apply(ipAddressStr: String) = {
    IpV6AddressParser(ipAddressStr)
  }
}

class IpV6Address(override val toString: String) extends IpAddressInternal {
  override val toUriString: String = s"[$toString]"
}

object IpV6AddressParser {
//  val DEC_OCTET_RGX = "25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9]"
//  val IPV4_ADDRESS_PATTERN = Pattern.compile(
//    "("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")\\.("+DEC_OCTET_RGX+")")

  def apply(ipv6AddressStr: String): IpV6Address = new IpV6Address(ipv6AddressStr)
//  {
//    val ipv4AddressMatcher = IpV4AddressParser.IPV4_ADDRESS_PATTERN.matcher(ipv4AddressStr)
//    if (ipv4AddressMatcher.matches()) {
//      IpV4Address(
//        UnsignedByte(ipv4AddressMatcher.group(1)),
//        UnsignedByte(ipv4AddressMatcher.group(2)),
//        UnsignedByte(ipv4AddressMatcher.group(3)),
//        UnsignedByte(ipv4AddressMatcher.group(4)))
//    } else {
//      throw new IpV4AddressParseException(ipv4AddressStr)
//    }
//  }
}