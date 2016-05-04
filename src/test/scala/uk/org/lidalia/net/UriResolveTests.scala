package uk.org.lidalia.net

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.net.Scheme.{http, urn}

class UriResolveTests extends FunSuite with TableDrivenPropertyChecks {


  val scenarios = Table(
    ("Relative Reference",                                                 "Expected"),
    ("g:h"           , "g:h"                ),
    ("g"             , "http://a/b/c/g"                ),
    ("./g"           , "http://a/b/c/g"                ),
    ("g/"            , "http://a/b/c/g/"                ),
    ("/g"            , "http://a/g"                ),
    ("//g"           , "http://g"                ),
    ("?y"            , "http://a/b/c/d;p?y"                ),
    ("g?y"           , "http://a/b/c/g?y"                ),
    ("#s"            , "http://a/b/c/d;p?q#s"                ),
    ("g#s"           , "http://a/b/c/g#s"                ),
    ("g?y#s"         , "http://a/b/c/g?y#s"                ),
    (";x"            , "http://a/b/c/;x"                ),
    ("g;x"           , "http://a/b/c/g;x"                ),
    ("g;x?y#s"       , "http://a/b/c/g;x?y#s"                ),
    (""              , "http://a/b/c/d;p?q"                ),
    ("."             , "http://a/b/c/"                ),
    ("./"            , "http://a/b/c/"                ),
    (".."            , "http://a/b/"                ),
    ("../"           , "http://a/b/"                ),
    ("../g"          , "http://a/b/g"                ),
    ("../.."         , "http://a/"                ),
    ("../../"        , "http://a/"                ),
    ("../../g"       , "http://a/g"                ),
    ("../../../g",    "http://a/g"),
    ("../../../../g", "http://a/g")

  )

  val uri = Uri("http://a/b/c/d;p?q")

  forAll(scenarios) { (relativeRef, expected) =>

    test(s"$relativeRef resolved correctly to $expected") {
      assert(uri.resolve(UriReference(relativeRef)) === Uri(expected))
    }
  }

}
