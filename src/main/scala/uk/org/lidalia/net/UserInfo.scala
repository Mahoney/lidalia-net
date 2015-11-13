package uk.org.lidalia
package net

import uk.org.lidalia.lang.RichObject
import uk.org.lidalia.scalalang.{PercentEncodedString, PercentEncodedStringFactory}
import uk.org.lidalia.net.UriConstants.{subDelims, unreserved}

object UserInfo {

  def apply(
    userInfoStr: String
  ) = {
    val parts = userInfoStr.split(":", 2)
    val username = UriUsername(parts(0))
    val password = if (parts.size > 1) Some(UriPassword(parts(1))) else None
    new UserInfo(username, password)
  }

  def apply(
    username: UriUsername,
    password: ?[UriPassword] = None
  ) = {
    new UserInfo(username, password)
  }

}

/**
 * Models a URI as defined in
 * <a href="http://tools.ietf.org/html/rfc3986#section-3.2.1">RFC 3986</a>.
 */
class UserInfo private (
  @Identity val username: UriUsername,
  @Identity val password: ?[UriPassword]) extends RichObject {

  override def toString = password.map(p => s"$username:$p").getOrElse(username.toString)

}

object UriUsername extends PercentEncodedStringFactory[UriUsername](unreserved ++ subDelims) {
  override def apply(usernameStr: String) = new UriUsername(usernameStr)
}

class UriUsername private (usernameStr: String)
  extends PercentEncodedString[UriUsername](UriUsername, usernameStr)

object UriPassword extends PercentEncodedStringFactory[UriPassword](unreserved ++ subDelims +':') {
  override def apply(passwordStr: String) = new UriPassword(passwordStr)
}

class UriPassword private (passwordStr: String)
  extends PercentEncodedString[UriPassword](UriPassword, passwordStr)
