package uk.org.lidalia.net

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.net.EqualsChecks.{equalsTest, possibleArgsFor, reflexiveTest}

class AuthorityTests extends PropSpec with TableDrivenPropertyChecks {

  property("Authority equals contract") {
    val argsForAuthority = possibleArgsFor(
        List(
          Some(UserInfo("user")),
          Some(UserInfo("user:password")),
          Some(UserInfo("user1")),
          Some(UserInfo("user:password1")),
          None
        ),
        List(
          HostAndPort("example.com"),
          HostAndPort("example2.com"),
          HostAndPort("1.2.3.4"),
          HostAndPort("1.2.3.5"),
          HostAndPort("[::1]"),
          HostAndPort("[2001:db8:0:0:0:ff00:42:8329]"),
          HostAndPort("[1.123]"),
          HostAndPort("[1.124]"),
          HostAndPort("example.com:80"),
          HostAndPort("example2.com:80"),
          HostAndPort("1.2.3.4:80"),
          HostAndPort("1.2.3.5:80"),
          HostAndPort("[::1]:80"),
          HostAndPort("[2001:db8:0:0:0:ff00:42:8329]:80"),
          HostAndPort("[1.123]:80"),
          HostAndPort("[1.124]:80")
        )
    )

    reflexiveTest(argsForAuthority) { (args) =>
      Authority(args._1, args._2)
    }

    equalsTest(argsForAuthority) { args =>
      Authority(args._1, args._2)
    }
  }
}
