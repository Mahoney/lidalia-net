package uk.org.lidalia
package net2

import uk.org.lidalia.lang.RichObject

object RegisteredName {
  def apply(nameStr: String) = new RegisteredName(nameStr)
}

class RegisteredName private(@Identity override val toString: String) extends RichObject with RegisteredNameInternal {
  override val toUriString = toString
}
