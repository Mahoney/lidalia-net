package uk.org.lidalia.net

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.net.EqualsChecks.{equalsTest, possibleArgsFor, reflexiveTest}

class HierarchicalPartTests extends PropSpec with TableDrivenPropertyChecks {

  property("HierarchicalPart equals contract") {
    val argsForHierarchicalPart = possibleArgsFor(
      List(
        "/path",
        "/",
        "",
        "/path1/path2",
        "//name/path",
        "//name/",
        "//name",
        "//name/path1/path2"
      )
    )

    reflexiveTest(argsForHierarchicalPart) { args =>
      HierarchicalPart(args)
    }

    equalsTest(argsForHierarchicalPart) { args =>
      HierarchicalPart(args)
    }
  }

  property("HierarchicalPartWithAuthority when part has authority") {

    val cases = Table(
      ("HierarchicalPart String", "Authority",       "Path"              ),
      ("//name/path",             Authority("name"), Path("/path")       ),
      ("//name/",                 Authority("name"), Path("/")           ),
      ("//name",                  Authority("name"), Path("")            ),
      ("//name/path1/path2",      Authority("name"), Path("/path1/path2"))
    )

    forAll(cases) { (hierPartStr, expectedAuthority, expectedPath) =>
      val hierPart = HierarchicalPart(hierPartStr)
      assert(hierPart.getClass === classOf[HierarchicalPartWithAuthority])
      assert(hierPart.authority === Some(expectedAuthority))
      assert(hierPart.path === expectedPath)
    }
  }

  property("HierarchicalPartPathOnly when part has no authority") {

    val cases = Table(
      ("HierarchicalPart String", "Path"                         ),
      ("/path",                   Path("/path")       ),
      ("/",                       Path("/")           ),
      ("",                        Path("")            ),
      ("/path1/path2",            Path("/path1/path2"))
    )

    forAll(cases) { (hierPartStr, expectedPath) =>
      val hierPart = HierarchicalPart(hierPartStr)
      assert(hierPart.getClass === classOf[HierarchicalPartPathOnly])
      assert(hierPart.authority === None)
      assert(hierPart.path === expectedPath)
    }
  }
}
