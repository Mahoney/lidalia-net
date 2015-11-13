package uk.org.lidalia.net

import EqualsChecks.{equalsTest, possibleArgsFor, reflexiveTest}
import org.scalatest
import org.scalatest.prop.TableDrivenPropertyChecks
import scalatest.PropSpec

class HostAndPortTests extends PropSpec with TableDrivenPropertyChecks {

  property("ResolvedHostAndPort equals") {
    val argsForResolvedHostAndPort = possibleArgsFor(
      List(Host("host1"), Host("host2")),
      List(Port(1), Port(2))
    )

    reflexiveTest(argsForResolvedHostAndPort) { (args) =>
      HostWithPort(args._1, args._2)
    }

    equalsTest(argsForResolvedHostAndPort) { (args) =>
      HostWithPort(args._1, args._2)
    }
  }

  property("HostWithoutPort equals") {
    val argsForHostWithoutPort = possibleArgsFor(List(Host("host1"), Host("host2")))

    reflexiveTest(argsForHostWithoutPort) { arg =>
      HostWithoutPort(arg)
    }

    equalsTest(argsForHostWithoutPort) { arg =>
      HostWithoutPort(arg)
    }
  }
}
