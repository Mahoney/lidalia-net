package uk.org.lidalia.net

import java.util.regex.Pattern

import UriConstants.{pctEncodedRegex, subDelimsRange, unreservedRange}

object HostAndPortParser {

  private val regNameOrIpv4 = s"($pctEncodedRegex|[$unreservedRange$subDelimsRange])*"
  private val ipLiteral = s"""\\[.*\\]"""
  private val port = "[0-9]+"
  private val hostAndPort = Pattern.compile(s"^($ipLiteral|$regNameOrIpv4)(:($port))?$$")

  def parse(hostAndPortStr: String): HostAndPort = {
    val matcher = hostAndPort.matcher(hostAndPortStr)
    if (matcher.matches() && matcher.group(4) != null) {
      HostWithPort(matcher.group(1), matcher.group(4))
    } else {
      HostWithoutPort(hostAndPortStr)
    }
  }
}
