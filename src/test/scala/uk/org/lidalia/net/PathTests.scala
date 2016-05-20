package uk.org.lidalia.net

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class PathTests extends FunSuite with TableDrivenPropertyChecks {

  test("path constructors") {

    assert(RelativePath()                == Path(""))
    assert(Path.root  == Path("/"))
    assert(RelativePath(Segment("a"), Segment(""))  == Path("a/"))
    assert(AbsolutePath(Segment("a")) == Path("/a"))
    assert(RelativePath(Segment("a"))               == Path("a"))
    assert(RelativePath(Segment("a"), Segment("b")) == Path("a/b"))

    assert(RelativePath(List())                == Path(""))
    assert(AbsolutePath(List(Segment("")))  == Path("/"))
    assert(RelativePath(List(Segment("a"), Segment("")))  == Path("a/"))
    assert(AbsolutePath(List(Segment("a"))) == Path("/a"))
    assert(RelativePath(List(Segment("a")))               == Path("a"))
    assert(RelativePath(List(Segment("a"), Segment("b"))) == Path("a/b"))

//    assert(Path(List(Segment("")))                == Path(Segment("")))
//    assert(Path(List(Segment(""),  Segment("")))  == Path(Segment(""),  Segment("")))
//    assert(Path(List(Segment("a"), Segment("")))  == Path(Segment("a"), Segment("")))
//    assert(Path(List(Segment(""),  Segment("a"))) == Path(Segment(""),  Segment("a")))
//    assert(Path(List(Segment("a")))               == Path(Segment("a")))
//    assert(Path(List(Segment("a"), Segment("b"))) == Path(Segment("a"), Segment("b")))
  }

//  test("cannot create with empty list") {
//    val e = intercept[IllegalArgumentException](Path(List()))
//    assert(e.getMessage == "requirement failed: Path must have at least one segment; segment can be empty")
//  }

  test("not equal to random list") {
    assert(RelativePath(Segment("a"), Segment("b")) != List(Segment("a"), Segment("b")))
    assert(AbsolutePath(Segment("a"), Segment("b")) != List(Segment("a"), Segment("b")))
  }

  test("path equals") {
    val differentPaths = List("", "/", "a/", "/a", "a", "a/b", "/a/b", "/a/b/")
    EqualsChecks.equalsTest(differentPaths) {
      Path(_)
    }
    EqualsChecks.reflexiveTest(differentPaths) {
      Path(_)
    }

    differentPaths.foreach { path =>
      assert(Path(path).toString() == path)
    }

    EqualsChecks.equalsTest(List(
        List(Segment("")),
        List(Segment(""),  Segment("")),
        List(Segment("a"), Segment("")),
        List(Segment(""),  Segment("a")),
        List(Segment("a"), Segment("b"))
      )) {
      RelativePath(_)
    }

    EqualsChecks.equalsTest(List(
      List(Segment("")),
      List(Segment(""),  Segment("")),
      List(Segment("a"), Segment("")),
      List(Segment(""),  Segment("a")),
      List(Segment("a"), Segment("b"))
    )) {
      AbsolutePath(_)
    }

    EqualsChecks.reflexiveTest(List(
      List(Segment("")),
      List(Segment(""),  Segment("")),
      List(Segment("a"), Segment("")),
      List(Segment(""),  Segment("a")),
      List(Segment("a"), Segment("b"))
    )) {
      AbsolutePath(_)
    }

    EqualsChecks.reflexiveTest(List(
      List(Segment("")),
      List(Segment(""),  Segment("")),
      List(Segment("a"), Segment("")),
      List(Segment(""),  Segment("a")),
      List(Segment("a"), Segment("b"))
    )) {
      RelativePath(_)
    }
  }

  val scenarios = Table(
    ("Path", "Expected"),
    ("/", "/"),
    ("/.", "/"),
    ("/./", "/"),
    ("/..", "/"),
    ("/../", "/"),
    ("/foo", "/foo"),
    ("/foo/", "/foo/"),
    ("/foo/.", "/foo/"),
    ("/foo/./", "/foo/"),
    ("/foo/..", "/"),
    ("/foo/../", "/"),
    ("/foo/bar", "/foo/bar"),
    ("/foo/bar/", "/foo/bar/"),
    ("/foo/bar/.", "/foo/bar/"),
    ("/foo/bar/./", "/foo/bar/"),
    ("/foo/bar/..", "/foo/"),
    ("/foo/bar/../", "/foo/"),

    ("/./foo", "/foo"),
    ("/./foo/", "/foo/"),
    ("/../foo", "/foo"),
    ("/../foo/", "/foo/"),

    ("/foo/./bar", "/foo/bar"),
    ("/foo/./bar/", "/foo/bar/"),
    ("/foo/../bar", "/bar"),
    ("/foo/../bar/", "/bar/")
  )

  forAll(scenarios) { (path, expected) =>
    test(s"$path resolved correctly to $expected") {
      val canonicalPath = Path(path).canonicalPath
      val expectedPath = Path(expected)
      assert(canonicalPath.toString() === expectedPath.toString())
    }
  }
}
