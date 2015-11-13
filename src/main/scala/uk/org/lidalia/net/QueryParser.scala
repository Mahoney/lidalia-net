package uk.org.lidalia.net

object QueryParser {
  def parse(queryStr: String): Query = {
    queryStr match {
      case "" => new Query(List())
      case _ =>
        val keyValuePairStrs = queryStr.toString.split("&", -1).toList

        val keyValuePairs = keyValuePairStrs.map { keyValuePairStr =>
          val keyValuePair = keyValuePairStr.split("=", 2).toList
          keyValuePair match {
            case List(key) => QueryParamKey(key) -> None
            case List(key, value) => QueryParamKey(key) -> Some(QueryParamValue(value))
          }
        }
        new Query(keyValuePairs)
    }
  }
}
