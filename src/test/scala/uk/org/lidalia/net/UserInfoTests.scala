package uk.org.lidalia.net

import org.scalatest.PropSpec
import uk.org.lidalia.net.EqualsChecks.{equalsTest, possibleArgsFor, reflexiveTest}

class UserInfoTests extends PropSpec {

  property("returns whole as username if no colon") {

    val userInfo = UserInfo("username")

    assert(userInfo.username === UriUsername("username"))
    assert(userInfo.password === None)
    assert(userInfo.toString === "username")
  }

  property("returns prior to colon as username") {

    val userInfo = UserInfo("username:password")

    assert(userInfo.username === UriUsername("username"))
    assert(userInfo.password === Some(UriPassword("password")))
    assert(userInfo.toString === "username:password")
  }

  property("colons allowed in password") {

    val userInfo = UserInfo("username:password:rest")

    assert(userInfo.username === UriUsername("username"))
    assert(userInfo.password === Some(UriPassword("password:rest")))
    assert(userInfo.toString === "username:password:rest")
  }

  property("empty password") {

    val userInfo = UserInfo("username:")

    assert(userInfo.username === UriUsername("username"))
    assert(userInfo.password === Some(UriPassword("")))
    assert(userInfo.toString === "username:")
  }

  property("@ not allowed") {

    val exception = intercept[IllegalArgumentException] {
      val userInfo = UserInfo("username@password")
    }

    assert(exception.getMessage == "requirement failed: UriUsername [username@password] must match (%[0-9A-Fa-f]{2}|[EeXsx8*4n\\-.9NjyT=YtJuUf&F!Aa5mM)Ii,;vG61VqQL'bgBlPp0_2CH+cWh(7rKwR$3k~ODZozSd])*")
  }

  property("equals and hashcode") {

    val argsForUserInfo = possibleArgsFor(
      List(
        UriUsername(""),
        UriUsername("user1"),
        UriUsername("user2")
      ),
      List(
        None,
        Some(UriPassword("")),
        Some(UriPassword("p1")),
        Some(UriPassword("p2"))
      )
    )

    reflexiveTest(argsForUserInfo) { (args) =>
      UserInfo(args._1, args._2)
    }

    equalsTest(argsForUserInfo) { (args) =>
      UserInfo(args._1, args._2)
    }
  }
}
