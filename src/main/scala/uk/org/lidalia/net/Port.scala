package uk.org.lidalia.net

object Port {

  def apply(portStr: String): Port = apply(portStr.toInt)

  def apply(portNumber: Int) = {
    require(portNumber >= 0 && portNumber <= 65535)
    new Port(portNumber)
  }

}

class Port private (val portNumber: Int) extends AnyVal {
  override def toString = portNumber.toString

  def isRestricted = portNumber < 1024
}
