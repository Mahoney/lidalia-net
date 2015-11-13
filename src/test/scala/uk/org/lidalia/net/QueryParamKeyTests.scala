package uk.org.lidalia.net

import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import uk.org.lidalia.scalalang.{TestUtils, EncodedStringChecks}
import TestUtils.{genNonEmptyStringFromChars, genRandomStringFrom, genStringFromChars}
import uk.org.lidalia.scalalang.EncodedStringChecks

class QueryParamKeyTests
    extends PropSpec
    with PropertyChecks {

  val queryChars: Set[Char] = UriConstants.pchar ++ Set('/', '?')

  val pctEncoded =
    for { num <- Gen.choose(0, 255) }
    yield "%"+String.format("%02X", num.asInstanceOf[Object])

  val otherThanPctEncoded =
    genStringFromChars(queryChars -- Set('&', '='))

  def genValidQueryStrings =
    genRandomStringFrom(
      pctEncoded,
      otherThanPctEncoded
    )

  property("Valid query param key strings accepted") {

    forAll((genValidQueryStrings, "query param key string")) { (queryString) =>
      whenever (!queryString.matches("%[^0-9A-F]|%[0-9A-F][^0-9A-F]|.*%$|%[0-9A-F]$") && queryString.size < 2048) {
        assert(QueryParamKey(queryString).toString === queryString)
      }
    }
  }

  property("Query param key string can be empty") {
    assert(QueryParamKey("").toString === "")
  }

  property("Query param key string cannot be null") {
    val exception = intercept[NullPointerException] {
      QueryParamKey(null)
    }
  }

  property("Invalid query param key string characters rejected") {

    val allChars: Set[Char] = (' ' to '~').toSet
    val invalidQueryChars = allChars -- queryChars

    forAll((genNonEmptyStringFromChars(invalidQueryChars), "query param key string")) { (queryString) =>
      whenever (!queryString.isEmpty && queryString.size < 1024) {
        val exception = intercept[IllegalArgumentException] {
          QueryParamKey(queryString)
        }
        assert(
          exception.getMessage ===
            s"requirement failed: QueryParamKey [$queryString] must match (%[0-9A-Fa-f]{2}|[EeXsx8*4n\\-.9NjyTYtJuUfF!Aa5mM)Ii@,;vG61VqQL'bgBlPp0?_2CH+cWh(7rKw:R$$3k~O/DZozSd])*"
        )
      }
    }
  }

  property("encode works") {
    assert(QueryParamKey.encode("/&=") === QueryParamKey("/%26%3D"))
  }

  property("decode works") {
    assert(QueryParamKey("%2F%26%3D").decode === "/&=")
  }

  property("encoded string contract") {
    EncodedStringChecks.checks(QueryParamKey)
  }

  property("equals checks") {

    EqualsChecks.equalsTest(List("a", "b")) {
      QueryParamKey(_)
    }

    EqualsChecks.reflexiveTest(List("a", "b")) {
      QueryParamKey(_)
    }

    EqualsChecks.equalsTest(List("=", "&")) {
      QueryParamKey.encode
    }

    EqualsChecks.reflexiveTest(List("=", "&")) {
      QueryParamKey.encode
    }
  }
}
