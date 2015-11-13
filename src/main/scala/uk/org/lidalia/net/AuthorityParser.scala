package uk.org.lidalia.net

object AuthorityParser {

  def parse(authorityStr: String): Authority = {
    val userInfoHostAndPort = authorityStr.split("@", 2)
    if (userInfoHostAndPort.size == 2) {
      Authority(
        UserInfo(userInfoHostAndPort(0)),
        HostAndPort(userInfoHostAndPort(1))
      )
    } else {
      Authority(
        hostAndPort = HostAndPort(userInfoHostAndPort(0))
      )
    }
  }
}
