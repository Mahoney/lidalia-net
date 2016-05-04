package uk.org.lidalia.net

import java.util.Locale

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class SchemeTests extends PropSpec with TableDrivenPropertyChecks {

  property("Valid scheme names accepted") {
    val validSchemeNames = Table(
      "Scheme name",
      "h",
      "h1",
      "h.",
      "h+",
      "h-",
      "Hh",
      "hH"
    )

    forAll(validSchemeNames) { validSchemeName =>
      assert(
        Scheme(validSchemeName).name === validSchemeName.toLowerCase(Locale.US)
      )
    }
  }

  property("Invalid scheme names rejected") {
    val invalidSchemeNames = Table(
      "Scheme name",
      "h ",
      " h",
      "1h",
      ".h",
      "+h",
      "-h",
      "h,",
      "h*",
      "h/",
      "h)",
      "h\nh",
      "\nh",
      "h\n",
      "h\r\nh",
      "\r\nh",
      "h\r\n"
    )

    forAll(invalidSchemeNames) { invalidSchemeName =>
      val exception = intercept[IllegalArgumentException] {
        Scheme(invalidSchemeName)
      }
      assert(
        exception.getMessage ===
          "requirement failed: " +
          s"SimpleScheme [$invalidSchemeName] " +
          "must match ^[a-zA-Z][a-zA-Z0-9\\+\\-\\.]*$"
      )
    }
  }

  property("Scheme with same name equal") {
    val scheme1 = Scheme("myscheme")
    val scheme2 = Scheme("myscheme")
    assert(scheme1 === scheme2)
    assert(scheme2 === scheme1)
  }

  property("Scheme with different name not equal") {
    val scheme1 = Scheme("myschemea")
    val scheme2 = Scheme("myschemeb")
    assert(scheme1 != scheme2)
    assert(scheme2 != scheme1)
  }

  property("Default ports") {
    assert(Scheme.http.defaultPort === Some(Port(80)))
    assert(Scheme("http").defaultPort === Some(Port(80)))
  }
}
