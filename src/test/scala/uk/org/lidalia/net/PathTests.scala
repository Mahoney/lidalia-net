package uk.org.lidalia.net

import org.scalatest.FunSuite

class PathTests extends FunSuite {

  test("path constructors") {

    assert(Path(Segment(""))                == Path(""))
    assert(Path(Segment(""),  Segment(""))  == Path("/"))
    assert(Path(Segment("a"), Segment(""))  == Path("a/"))
    assert(Path(Segment(""),  Segment("a")) == Path("/a"))
    assert(Path(Segment("a"))               == Path("a"))
    assert(Path(Segment("a"), Segment("b")) == Path("a/b"))

    assert(Path(List(Segment("")))                == Path(""))
    assert(Path(List(Segment(""),  Segment("")))  == Path("/"))
    assert(Path(List(Segment("a"), Segment("")))  == Path("a/"))
    assert(Path(List(Segment(""),  Segment("a"))) == Path("/a"))
    assert(Path(List(Segment("a")))               == Path("a"))
    assert(Path(List(Segment("a"), Segment("b"))) == Path("a/b"))

    assert(Path(List(Segment("")))                == Path(Segment("")))
    assert(Path(List(Segment(""),  Segment("")))  == Path(Segment(""),  Segment("")))
    assert(Path(List(Segment("a"), Segment("")))  == Path(Segment("a"), Segment("")))
    assert(Path(List(Segment(""),  Segment("a"))) == Path(Segment(""),  Segment("a")))
    assert(Path(List(Segment("a")))               == Path(Segment("a")))
    assert(Path(List(Segment("a"), Segment("b"))) == Path(Segment("a"), Segment("b")))
  }

  test("cannot create with empty list") {
    val e = intercept[IllegalArgumentException](Path(List()))
    assert(e.getMessage == "requirement failed: Path must have at least one segment; segment can be empty")
  }

  test("not equal to random list") {
    assert(Path(Segment("a"), Segment("b")) != List(Segment("a"), Segment("b")))
  }

  test("path equals") {
    EqualsChecks.equalsTest(List("", "/", "a/", "/a", "a", "a/b")) {
      Path(_)
    }
    EqualsChecks.reflexiveTest(List("", "/", "a/", "/a", "a", "a/b")) {
      Path(_)
    }

    EqualsChecks.equalsTest(List(
        List(Segment("")),
        List(Segment(""),  Segment("")),
        List(Segment("a"), Segment("")),
        List(Segment(""),  Segment("a")),
        List(Segment("a"), Segment("b"))
      )) {
      Path(_)
    }

    EqualsChecks.reflexiveTest(List(
      List(Segment("")),
      List(Segment(""),  Segment("")),
      List(Segment("a"), Segment("")),
      List(Segment(""),  Segment("a")),
      List(Segment("a"), Segment("b"))
    )) {
      Path(_)
    }
  }

}
