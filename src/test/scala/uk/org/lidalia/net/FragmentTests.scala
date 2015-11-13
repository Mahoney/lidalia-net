package uk.org.lidalia.net

import org.scalatest.FunSuite
import uk.org.lidalia.scalalang.EncodedStringChecks

class FragmentTests extends FunSuite {

  test("normal encoded string checks") {
    EncodedStringChecks.checks(Fragment)
  }
}
