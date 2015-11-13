package uk.org.lidalia.net

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class UrlTests extends PropSpec with TableDrivenPropertyChecks {

  property("error if called with a URI that is not a URL") {
    val exception = intercept[IllegalArgumentException] {
      Url("urn:example:animal")
    }
    assert(
      exception.getMessage ===
        "urn:example:animal is not a URL - it has no authority"
    )
  }

  property("resolved port is specific port when port provided") {

    val schemes = Table(
      "Scheme",
      "http",
      "https",
      "ws",
      "wss",
      "unknown"
    )

    val port = Port(12345)

    forAll(schemes) { scheme =>
      val url = Url(s"$scheme://example.com:$port")
      assert(url.resolvedPort === port)
    }
  }

  property("resolved port is default port when port omitted") {

    val schemesWithDefaultPorts = Table(
      ("Scheme", "Default port"),
      ("http",   80            ),
      ("https",  443           )
    )

    val port = Port(12345)

    forAll(schemesWithDefaultPorts) { (scheme, port) =>
      val url = Url(s"$scheme://example.com")
      assert(url.resolvedPort === Port(port))
    }
  }

  property("resolved port throws exception if no default port and port omitted") {

    val exception = intercept[IllegalStateException] {
      Url("unknown://example.com").resolvedPort
    }
    assert(
      exception.getMessage ===
        "No port specified and no default port for scheme in unknown://example.com"
    )
  }
}
