package uk.org.lidalia
package net2

object HierarchicalPart {
  def apply(hierarchicalPartStr: String): HierarchicalPart
    = HierarchicalPartParser.parse(hierarchicalPartStr)
}

sealed abstract class HierarchicalPart {
  val authority: ?[Authority]
  val path: Path

  def canEqual(other: Any): Boolean = other.isInstanceOf[HierarchicalPart]

  override def equals(other: Any): Boolean = other match {
    case that: HierarchicalPart =>
      (that canEqual this) &&
        path == that.path &&
        authority == that.authority
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(path, authority)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object HierarchicalPartWithAuthority {
  def apply(authority: Authority,
            path: Path) = new HierarchicalPartWithAuthority(authority, path)
}

final class HierarchicalPartWithAuthority private(
               override val authority: Some[Authority],
               override val path: Path) extends HierarchicalPart {

  override def toString = "//"+authority.get+path

}

object HierarchicalPartPathOnly {
  def apply(path: Path) = new HierarchicalPartPathOnly(path)
}

final class HierarchicalPartPathOnly private(override val path: Path) extends HierarchicalPart {

  override val authority = None

  override def toString = path.toString
}
