package uk.org.lidalia
package net2

import uk.org.lidalia.net2.UriConstants.split

private object UriParser {

  def apply(uriStr: String): Uri = {
    try {
      val schemeAndRest = uriStr.split(":", 2)
      val scheme = Scheme(schemeAndRest(0))

      val hierarchicalPartAndRest = parseHierarchicalPartAndRest(schemeAndRest(1))
      val hierarchicalPart = HierarchicalPart(hierarchicalPartAndRest._1)

      val queryAndOrFragmentStr = hierarchicalPartAndRest._2
      val queryAndFragment = queryAndOrFragmentStr.map(parseQueryAndFragment) getOrElse (None, None)
      val query = queryAndFragment._1
      val fragment = queryAndFragment._2

      Uri(
        scheme,
        hierarchicalPart,
        query,
        fragment
      )
    } catch {
      case e: Throwable =>
        throw new UriParseException(
          s"[$uriStr] is not a valid URI",
          e
        )
    }
  }

  private def parseHierarchicalPartAndRest(hierarchicalPartAndRest: String) = {
    if (hierarchicalPartAndRest.startsWith("?") || hierarchicalPartAndRest.startsWith("#")) {
      ("", Some(hierarchicalPartAndRest))
    } else {
      split(hierarchicalPartAndRest, "(?=[\\?#])")
    }
  }

  private def parseQueryAndFragment(qf: String): (Option[Query], Option[Fragment]) = {
    if (qf.startsWith("?")) {
      val queryAndFragmentStr = split(qf.substring(1), "#")
      val fragment = queryAndFragmentStr._2.map(Fragment(_))
      val query = Query(queryAndFragmentStr._1)
      (Some(query), fragment)
    } else if (qf.startsWith("#")) {
      val fragment = Fragment(qf.substring(1))
      (None, Some(fragment))
    } else {
      (None, None)
    }
  }
}

class UriParseException(
                         message: String,
                         cause: Throwable
                         ) extends RuntimeException(
  message,
  cause
)
