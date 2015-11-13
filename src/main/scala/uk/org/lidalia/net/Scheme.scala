package uk.org.lidalia
package net

import java.util.Locale

import uk.org.lidalia.scalalang.RegexVerifiedWrappedString

/**
 * Models a Scheme as defined in
 * <a href="http://tools.ietf.org/html/rfc3986#section-3.1">
 *   RFC 3986 Section 3.1
 * </a>
 * with the recognition that many schemes in URLS have conventional default
 * ports.
 */
object Scheme {

  private val VALID_SCHEME_REGEX = "^[a-zA-Z][a-zA-Z0-9\\+\\-\\.]*$".r.pattern

  val HTTP   = Scheme("http",  Port(80))
  val HTTPS  = Scheme("https", Port(443))
  val FTP    = Scheme("ftp",   Port(21))
  val SSH    = Scheme("ssh",   Port(22))
  val MAILTO = new SimpleScheme("mailto")
  val FILE   = new SimpleScheme("file")
  val URN   = new SimpleScheme("urn")

  private val knownSchemes = List(
    HTTP,
    HTTPS,
    FTP,
    SSH,
    MAILTO,
    FILE
  ).map{ scheme => scheme.name -> scheme }.toMap

  def apply(name: String): Scheme =
    knownSchemes.getOrElse(
      name.toLowerCase(Locale.US),
      new SimpleScheme(name)
    )

  def apply(
        name: String,
        defaultPort: Port
      ): SchemeWithDefaultPort =
    new SchemeWithDefaultPort(
        name,
        defaultPort
    )
}

sealed abstract class Scheme(
        mixedCaseName: String
    )
    extends RegexVerifiedWrappedString(mixedCaseName, Scheme.VALID_SCHEME_REGEX)
    with Immutable {

  val name: String = mixedCaseName.toLowerCase(Locale.US)
  val defaultPort: ?[Port]

}

final class SchemeWithDefaultPort private[net](
      mixedCaseName: String,
      val defaultPort: Some[Port]
    )
    extends Scheme(mixedCaseName)

final class SimpleScheme private[net](
      mixedCaseName: String
    )
    extends Scheme(mixedCaseName) {

  val defaultPort = None

}
