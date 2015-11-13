package uk.org.lidalia.net

import Scheme.{HTTP, URN}
import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.net.EqualsChecks.{equalsTest, possibleArgsFor, reflexiveTest}

class UriTests extends PropSpec with TableDrivenPropertyChecks {

  property("Uri equals contract") {

    val argsForUri = possibleArgsFor(
      List(
        Scheme("scheme1"),
        Scheme("scheme2")
      ),
      List(
        HierarchicalPart("//host1"),
        HierarchicalPart("//host2"),
        HierarchicalPart("example:animal:ferret:nose")
      ),
      List(
        Some(Query("a=b")),
        Some(Query("c=d")),
        None
      ),
      List(
        Some(Fragment("anchor1")),
        Some(Fragment("anchor2")),
        None
      )
    )

    reflexiveTest(argsForUri) { (args) =>
      Uri(args._1, args._2, args._3, args._4)
    }

    equalsTest(argsForUri) { (args) =>
      Uri(args._1, args._2, args._3, args._4)
    }
  }

  property("Valid URIs maintained") {
    val validUris = Table(
      "URI",
      "foo://example.com:8042/over/there?name=ferret#nose",
      "foo://example.com:8042/over/there?name=ferret",
      "foo://example.com:8042/over/there",
      "foo://example.com:8042/over/there#nose",
      "foo://example.com:8042/over/there#",
      "foo://example.com:8042/over/there?",
      "foo://example.com:8042/over/there?#",
      "foo://example.com:8042/over/there?blah#",
      "foo://example.com:8042/over/there?#blah",
      "foo://example.com:8042?name=ferret#nose",
      "foo://example.com:8042?name=ferret",
      "foo://example.com:8042",
      "foo://example.com:8042/",
      "foo://example.com:8042/?#",
      "foo://example.com:8042#nose",
      "foo://example.com:8042#",
      "foo://example.com:8042?",
      "foo://example.com:8042?#",
      "foo://example.com:8042?blah#",
      "foo://example.com:8042?#blah",
      "foo://1.1.1.1:8042?#blah",
      "foo://[1.1.1.1]:8042?#blah",
      "foo://[2001:cdba::3257:9652]:8042?#blah",
      "foo:?query#fragment",
      "foo:path?query#fragment",
      "foo:path/path2?query#fragment",
      "foo:/path2/path3?query#fragment",
      "urn:example:animal?ferret#nose",
      "urn:example:animal?ferret",
      "urn:example:animal?ferret",
      "urn:example:animal#nose",
      "urn:example:animal"
    )

    forAll(validUris) { uri =>
      assert(
        Uri(uri).toString === uri
      )
    }
  }

  property("Uri parsed correctly") {
    val uris = Table(
      ("URI",                                                 "Scheme", "Authority",              "Path",           "Query",             "Fragment"),
      ("http://example.com:8042/over/there?name=ferret#nose", HTTP,     Some("example.com:8042"), "/over/there",    Some("name=ferret"), Some("nose")),
      ("http://example.com:8042/over/there?name=ferret",      HTTP,     Some("example.com:8042"), "/over/there",    Some("name=ferret"), None),
      ("http://example.com:8042/over/there",                  HTTP,     Some("example.com:8042"), "/over/there",    None,                None),
      ("http://example.com:8042/over/there#nose",             HTTP,     Some("example.com:8042"), "/over/there",    None,                Some("nose")),
      ("urn:example:animal?ferret#nose",                      URN,      None,                     "example:animal", Some("ferret"),      Some("nose")),
      ("urn:example:animal?ferret",                           URN,      None,                     "example:animal", Some("ferret"),      None),
      ("urn:example:animal#nose",                             URN,      None,                     "example:animal", None,                Some("nose")),
      ("urn:example:animal#",                                 URN,      None,                     "example:animal", None,                Some("")),
      ("urn:example:animal?",                                 URN,      None,                     "example:animal", Some(""),            None),
      ("urn:example:animal?#",                                URN,      None,                     "example:animal", Some(""),            Some("")),
      ("urn:example:animal?blah#",                            URN,      None,                     "example:animal", Some("blah"),        Some("")),
      ("urn:example:animal?#blah",                            URN,      None,                     "example:animal", Some(""),            Some("blah")),
      ("urn:example:animal",                                  URN,      None,                     "example:animal", None,                None),

      ("http://example.com:8042?name=ferret#nose",            HTTP,     Some("example.com:8042"), "",               Some("name=ferret"), Some("nose")),
      ("http://example.com?name=ferret#nose",                 HTTP,     Some("example.com"),      "",               Some("name=ferret"), Some("nose")),
      ("http://example.com:8042?name=ferret",                 HTTP,     Some("example.com:8042"), "",               Some("name=ferret"), None),
      ("http://example.com?name=ferret",                      HTTP,     Some("example.com"),      "",               Some("name=ferret"), None),
      ("http://example.com:8042",                             HTTP,     Some("example.com:8042"), "",               None,                None),
      ("http://example.com",                                  HTTP,     Some("example.com"),      "",               None,                None),
      ("http://example.com:8042/",                            HTTP,     Some("example.com:8042"), "/",              None,                None),
      ("http://example.com/",                                 HTTP,     Some("example.com"),      "/",              None,                None),
      ("http:?query#fragment",                                HTTP,     None,                     "",               Some("query"),       Some("fragment")),
      ("http:path?query#fragment",                            HTTP,     None,                     "path",           Some("query"),       Some("fragment")),
      ("http:/path?query#fragment",                           HTTP,     None,                     "/path",          Some("query"),       Some("fragment")),
      ("http:path/path2?query#fragment",                      HTTP,     None,                     "path/path2",     Some("query"),       Some("fragment"))
    )

    forAll(uris) { (uriString,
                    scheme,
                    authority,
                    path,
                    queryStr,
                    fragmentStr) =>
      val uri = Uri(uriString)
      assert(uri.scheme === scheme)
      assert(uri.hierarchicalPart.authority === authority.map(Authority(_)))
      assert(uri.hierarchicalPart.path.toString === path)
      assert(uri.query === queryStr.map(Query(_)))
      assert(uri.fragment === fragmentStr.map(Fragment(_)))
    }
  }

  property("Invalid uris rejected") {
    val invalidUrisAndErrorMessages = Table(
      ("URI",   "Error message"),
      ("http",  "[http] is not a valid URI"),
      (".http", "[.http] is not a valid URI")
    )

    forAll(invalidUrisAndErrorMessages) { (invalidUri,
                                           expectedErrorMessage) =>
      val exception = intercept[UriParseException] {
        Uri(invalidUri)
      }

      assert(
        exception.getMessage === expectedErrorMessage
      )
    }
  }

  property("Absolute uri strips fragment") {

    val uris = Table(
      ("URI with fragment",                                   "URI without fragment"                          ),
      ("http://example.com:8042/over/there?name=ferret#nose", "http://example.com:8042/over/there?name=ferret"),
      ("http://example.com:8042/over/there#nose",             "http://example.com:8042/over/there"            ),
      ("http:?query#fragment",                                "http:?query"                                   ),
      ("http:path?query#fragment",                            "http:path?query"                               ),
      ("http:/path?query#fragment",                           "http:/path?query"                              ),
      ("http:path/path2?query#fragment",                      "http:path/path2?query"                         ),
      ("http:path/path2?query#",                              "http:path/path2?query"                         )
    )

    forAll(uris) { (uriWithFragment, uriWithoutFragment) =>
      assert(Uri(uriWithFragment).absoluteUri === Uri(uriWithoutFragment))
    }
  }

  property("Absolute uri does nothing to uri with no fragment") {

    val uris = Table(
      "URI Without fragment",
      "http://example.com:8042/over/there?name=ferret",
      "http://example.com:8042/over/there",
      "http:?query",
      "http:path?query",
      "http:/path?query",
      "http:path/path2?query"
    )

    forAll(uris) { uriWithoutFragment =>
      val uri = Uri(uriWithoutFragment)
      assert(uri.absoluteUri === uri)
    }
  }
}
