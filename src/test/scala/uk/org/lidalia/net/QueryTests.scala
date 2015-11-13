package uk.org.lidalia.net

import java.util.regex.Pattern

import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import uk.org.lidalia.scalalang.{TestUtils, EncodedStringChecks}
import TestUtils.{genNonEmptyStringFromChars, genRandomStringFrom, genStringFromChars}
import uk.org.lidalia.scalalang.EncodedStringChecks

class QueryTests
    extends PropSpec
    with PropertyChecks {

  val queryChars: Set[Char] = UriConstants.pchar ++ Set('/', '?')

  val pctEncoded =
    for { num <- Gen.choose(0, 255) }
    yield "%"+String.format("%02X", num.asInstanceOf[Object])

  val otherThanPctEncoded =
    genStringFromChars(queryChars)

  def genValidQueryStrings =
    genRandomStringFrom(
      pctEncoded,
      otherThanPctEncoded
    )

  property("Valid query strings accepted") {

    forAll((genValidQueryStrings, "query string")) { (queryString) =>
      whenever (queryString.size < 2048 && (!queryString.contains("%") || queryString.matches(".*%[0-9A-Fa-f]{2}.*"))) {
        assert(Query(queryString).toString === queryString)
      }
    }
  }

  property("Query string can be empty") {
    assert(Query("").toString === "")
  }

  property("Query string cannot be null") {
    val exception = intercept[NullPointerException] {
      Query(null)
    }
  }

  property("Invalid query string characters rejected") {

    val allChars: Set[Char] = (' ' to '~').toSet
    val invalidQueryChars = allChars -- queryChars

    forAll((genNonEmptyStringFromChars(invalidQueryChars), "query string")) { (queryString) =>
      whenever (!queryString.isEmpty && queryString.size < 1024) {
        val exception = intercept[IllegalArgumentException] {
          Query(queryString)
        }
      }
    }
  }

  property("hex digit") {
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("0").matches())
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("9").matches())
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("A").matches())
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("a").matches())
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("F").matches())
    assert(Pattern.compile(UriConstants.hexDigitRegex).matcher("f").matches())
    assert(!Pattern.compile(UriConstants.hexDigitRegex).matcher("G").matches())
    assert(!Pattern.compile(UriConstants.hexDigitRegex).matcher("g").matches())
  }

  property("2 hex digits") {
    val valid = Table(
      "String",
      "%00",
      "%F0",
      "%0F",
      "%f0",
      "%0f"
    )
    forAll(valid) { validhex =>
      assert(Pattern.compile(UriConstants.pctEncodedRegex).matcher(validhex).matches())
    }
    val invalid = Table(
      "String",
      "%G0",
      "%0G",
      "%000",
      "%FFF",
      "%0",
      "%F"
    )
    forAll(invalid) { invalidhex =>
      assert(!Pattern.compile(UriConstants.pctEncodedRegex).matcher(invalidhex).matches())
    }
  }

  property("queryChars") {
    val valid = Table(
      "String",
      "a",
      "z",
      "0",
      "9",
      "A",
      "Z",
      "-",
      ".",
      "_",
      "~",
      "!",
      "$",
      "&",
      "(",
      ")",
      "*",
      "+",
      ",",
      ":",
      "=",
      ";",
      "@"
    )
    forAll(valid) { validq =>
      assert(UriConstants.Patterns.query.matcher(validq).matches())
    }
    val allChars: Set[String] = (' ' to '~').map(""+_).toSet
    val invalidQueryChars: Set[String] = allChars -- queryChars.map(""+_).toSet
    val invalid = Table(
      "String",
      invalidQueryChars.toSeq:_*
    )
    forAll(invalid) { invalidq =>
      assert(!Pattern.compile(UriConstants.pctEncodedRegex).matcher(invalidq).matches())
    }
  }

  property("params are maintained") {
    val queryStringParamFormats = Table(
      "Query String Param Format",
      "&&",
      "&=",
      "=&",
      "==",
      "a&a=b",
      "a=a&b",
      "a=",
      "=a",
      "&a",
      "a&",
      "a=b&a=c&a2=b&a=d",
      "&"
    )

    forAll(queryStringParamFormats) { queryStringParamFormat =>
      assert(Query(queryStringParamFormat).toString === queryStringParamFormat)
    }
  }

  property("query parameters parsed") {
    val queryStringsToParameters = Table(
      ("Query String",       "Expected parameter map"),
      ("",                    Map()                  ),
      ("foo",                 Map(
                                QueryParamKey("foo") -> List()
                              )
      ),
      ("foo=",                Map(
                                QueryParamKey("foo") -> List(QueryParamValue(""))
                              )
      ),
      ("foo&",                Map(
                                QueryParamKey("foo") -> List(),
                                QueryParamKey("")    -> List()
                              )
      ),
      ("&foo",                Map(
                                QueryParamKey("")    -> List(),
                                QueryParamKey("foo") -> List()
                              )
      ),
      ("=foo",                Map(
                                QueryParamKey("")    -> List(QueryParamValue("foo"))
                              )
      ),
      ("&",                   Map(
                                QueryParamKey("")    -> List()
                              )
      ),
      ("&&",                  Map(
                                QueryParamKey("")    -> List()
                              )
      ),
      ("=&=&",                Map(
                                QueryParamKey("")    -> List(
                                                          QueryParamValue(""),
                                                          QueryParamValue("")
                                                       )
                              )
      ),
      ("foo&a=1&b=2&a=3&bar", Map(
                                QueryParamKey("foo") -> List(),
                                QueryParamKey("a")   -> List(
                                                          QueryParamValue("1"),
                                                          QueryParamValue("3")
                                                       ),
                                QueryParamKey("b")   -> List(
                                                          QueryParamValue("2")
                                                       ),
                                QueryParamKey("bar") -> List()
                              )
      ),
      ("a&a=1&a&a=&a=2",      Map(
                                QueryParamKey("a")   -> List(
                                                          QueryParamValue("1"),
                                                          QueryParamValue(""),
                                                          QueryParamValue("2")
                                                       )
                              )
      ),
      ("a=1=2",               Map(
                                QueryParamKey("a")   -> List(
                                                          QueryParamValue("1=2")
                                                       )
                              )
      )
    )

    forAll(queryStringsToParameters) { (queryString, expectedParameterMap) =>
      assert(Query(queryString).paramMap === expectedParameterMap)
      assert(Query(queryString).toString === queryString)
    }
  }

  val queryStringsToParameters = Table(
    ("Query String", "Value of a"),
    ("a=b",          List(QueryParamValue("b"))),
    ("a=b&a=c",      List(QueryParamValue("b"), QueryParamValue("c"))),
    ("",             List())
  )

  property("can get all values for a param key") {
    forAll(queryStringsToParameters) { (queryString, valueOfParam) =>
      assert(Query(queryString).get("a") === valueOfParam)
    }
  }

  property("can get all values for a param key using domain objects") {
    forAll(queryStringsToParameters) { (queryString, valueOfParam) =>
      assert(Query(queryString).get(QueryParamKey("a")) === valueOfParam)
    }
  }

  val queryStringsToValueOfA = Table(
    ("Query String", "Value of a"),
    ("a=b",          Some(QueryParamValue("b"))),
    ("a=b&a=c",      Some(QueryParamValue("b"))),
    ("b=a",          None),
    ("",             None)
  )

  property("can get a single value for a param key") {
    forAll(queryStringsToValueOfA) { (queryString, valueOfParam) =>
      assert(Query(queryString).getFirst("a") === valueOfParam)
    }
  }

  property("can get a single value for a param key using domain object") {
    forAll(queryStringsToValueOfA) { (queryString, valueOfParam) =>
      assert(Query(queryString).getFirst(QueryParamKey("a")) === valueOfParam)
    }
  }

  property("can get all decoded values for param key") {
    val query = Query("a=b%2F")
    assert(query.getDecoded("a") === List("b/"))
  }

  property("can remove all values for a param key") {
    val queryStringsToAfterRemove = Table(
      ("Query String Before removing a", "Query String after removing a"),
      ("foo",                            "foo"),
      ("foo=",                           "foo="),
      ("foo&",                           "foo&"),
      ("&foo",                           "&foo"),
      ("=foo",                           "=foo"),
      ("&",                              "&"),
      ("&&",                             "&&"),
      ("=&=&",                           "=&=&"),
      ("foo&a=1&b=2&a=3&bar",            "foo&b=2&bar"),
      ("a&a=1&a&a=&a=2",                 ""),
      ("a=1=2",                          "")
    )

    forAll(queryStringsToAfterRemove) { (queryString, expectedAfterRemove) =>
      assert(Query(queryString) - "a" === Query(expectedAfterRemove))
    }
  }

  val queryStringsToAfterRemove = Table(
    ("Query String Before removing a=1", "Query String after removing a=1"),
    ("foo",                              "foo"),
    ("foo=",                             "foo="),
    ("foo&",                             "foo&"),
    ("&foo",                             "&foo"),
    ("=foo",                             "=foo"),
    ("&",                                "&"),
    ("&&",                               "&&"),
    ("=&=&",                             "=&=&"),
    ("foo&a=1&b=2&a=3&bar",              "foo&b=2&a=3&bar"),
    ("a&a=1&a&a=&a=2",                   "a&a&a=&a=2"),
    ("a=1=2",                            "a=1=2")
  )

  property("can remove a single value for a param key") {
    forAll(queryStringsToAfterRemove) { (queryString, expectedAfterRemove) =>
      assert(Query(queryString) - ("a", "1") === Query(expectedAfterRemove))
    }
  }

  property("can remove a single value for a param key using domain objects") {
    forAll(queryStringsToAfterRemove) { (queryString, expectedAfterRemove) =>
      assert(Query(queryString) - (QueryParamKey("a"), QueryParamValue("1")) === Query(expectedAfterRemove))
    }
  }

  property("can remove multiple values for a param key") {
    assert(Query("a=1&b&a=2&a&a=3") - ("a", "1", "2") === Query("b&a&a=3"))
  }

  property("can add a param with multiple values") {
    val queryStringsToAfterRemove = Table(
      ("Query String Before adding a=1", "Query String after adding a=1"),
      ("",                               "a=1&a=2"),
      ("foo",                            "foo&a=1&a=2"),
      ("foo=",                           "foo=&a=1&a=2"),
      ("foo&",                           "foo&&a=1&a=2"),
      ("&foo",                           "&foo&a=1&a=2"),
      ("=foo",                           "=foo&a=1&a=2"),
      ("&",                              "&&a=1&a=2"),
      ("&&",                             "&&&a=1&a=2"),
      ("=&=&",                           "=&=&&a=1&a=2"),
      ("foo&a=1&b=2&a=3&bar",            "foo&a=1&b=2&a=3&bar&a=1&a=2"),
      ("a&a=1&a&a=&a=2",                 "a&a=1&a&a=&a=2&a=1&a=2"),
      ("a=1=2",                          "a=1=2&a=1&a=2")
    )

    forAll(queryStringsToAfterRemove) { (queryString, expectedAfterAdd) =>
      assert(Query(queryString) ++ ("a", "1", "2") === Query(expectedAfterAdd))
    }
  }

  property("can set a param with multiple values") {
    val queryStringsToAfterRemove = Table(
      ("Query String Before adding a=1", "Query String after adding a=1"),
      ("",                               "a=1&a=2"),
      ("foo",                            "foo&a=1&a=2"),
      ("foo=",                           "foo=&a=1&a=2"),
      ("foo&",                           "foo&&a=1&a=2"),
      ("&foo",                           "&foo&a=1&a=2"),
      ("=foo",                           "=foo&a=1&a=2"),
      ("&",                              "&&a=1&a=2"),
      ("&&",                             "&&&a=1&a=2"),
      ("=&=&",                           "=&=&&a=1&a=2"),
      ("foo&a=1&b=2&a=3&bar",            "foo&b=2&bar&a=1&a=2"),
      ("a&a=1&a&a=&a=2",                 "a=1&a=2"),
      ("a=1=2",                          "a=1&a=2")
    )

    forAll(queryStringsToAfterRemove) { (queryString, expectedAfterAdd) =>
      assert(Query(queryString).set("a", "1", "2") === Query(expectedAfterAdd))
    }
  }

  property("encode works") {
    assert(Query.encode("hello &param#=value#") === Query("hello%20&param%23=value%23"))
  }

  property("decode works") {
    assert(Query("%2F").decode === "/")
  }

  property("encoded string contract") {
    EncodedStringChecks.checks(Query)
  }
}
