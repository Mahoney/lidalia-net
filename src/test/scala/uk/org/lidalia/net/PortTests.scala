package uk.org.lidalia.net

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class PortTests extends PropSpec with TableDrivenPropertyChecks {


  property("a Port cannot be constructed with invalid string input") {
    val invalidPortStrings =
      Table(
        ("input to constructor", "expected exception type"        ),
        ("not a number",         classOf[NumberFormatException]   ),
        (null,                   classOf[NumberFormatException]   ),
        ("-1",                   classOf[IllegalArgumentException]),
        ("65536",                classOf[IllegalArgumentException])
      )
    forAll(invalidPortStrings) { (input, expectedExceptionType) =>
      val e = intercept[Exception] {
        Port(input)
      }
      assert(e.getClass == expectedExceptionType)
    }
  }

  property("a Port cannot be constructed with invalid int input") {
    val invalidPortNumbers =
      Table(
        "input to constructor",
        -1,
        65536
      )
    forAll(invalidPortNumbers) { input =>
      intercept[IllegalArgumentException] {
        Port(input)
      }
    }
  }

  property("a Port can be constructed with valid string input") {
    val validPortStrings = Table(
      "input to constructor",
      "0",
      "65535"
    )
    forAll(validPortStrings) { input =>
      assert(Port(input).toString === input)
    }
  }

  property("a Port can be constructed with valid int input") {
    val validPortNumbers = Table(
      "input to constructor",
      0,
      65535
    )
    forAll(validPortNumbers) { input =>
      assert(Port(input).portNumber === input)
    }
  }

  property("equals") {
    assert(Port(80) === Port(80))
    assert(Port("80") === Port("80"))
    assert(Port(80) === Port("80"))
    assert(Port("80") === Port(80))

    assert(Port(80) != Port(81))
    assert(Port("80") != Port("81"))
    assert(Port(80) != Port("81"))
    assert(Port("80") != Port(81))
  }

  property("hashCode") {
    assert(Port(80).hashCode === Port(80).hashCode)
    assert(Port(80).hashCode != Port(81).hashCode)
  }

  property("is restricted") {
    val expectedPortRestrictedStatuses = Table(
      ("port", "should be restricted"),
      (0,      true),
      (1023,   true),
      (1024,   false),
      (65535,  false)
    )

    forAll(expectedPortRestrictedStatuses) { (portNumber, expected) =>
      assert(Port(portNumber).isRestricted == expected)
    }
  }
}
