package uk.org.lidalia.net

object UriReference {
  def apply(ref: String): UriReference = {

    UriParser.parseUriReference(ref)
  }
}
trait UriReference {

  private [net] def resolveTo(uri: Uri): Uri

}
