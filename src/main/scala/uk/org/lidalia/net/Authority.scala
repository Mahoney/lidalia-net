package uk.org.lidalia
package net

object Authority {
  def apply(authorityStr: String): Authority
    = AuthorityParser.parse(authorityStr)

  def apply(
    userInfo: ?[UserInfo] = None,
    hostAndPort: HostAndPort
  ) = new Authority(userInfo, hostAndPort)
}

class Authority private(val userInfo: ?[UserInfo],
                        val hostAndPort: HostAndPort) {

  override def toString
    = userInfo.map(_+"@"+hostAndPort) getOrElse hostAndPort.toString


  def canEqual(other: Any): Boolean = other.isInstanceOf[Authority]

  override def equals(other: Any): Boolean = other match {
    case that: Authority =>
      (that canEqual this) &&
          userInfo == that.userInfo &&
          hostAndPort == that.hostAndPort
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(userInfo, hostAndPort)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
