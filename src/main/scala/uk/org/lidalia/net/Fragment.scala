package uk.org.lidalia.net

import uk.org.lidalia.scalalang.{PercentEncodedString, PercentEncodedStringFactory}

object Fragment extends PercentEncodedStringFactory[Fragment](UriConstants.pchar++Set('/', '?')) {

  def apply(fragmentStr: String) = new Fragment(fragmentStr)

}

class Fragment private(fragmentStr: String)
    extends PercentEncodedString[Fragment](Fragment, fragmentStr)
