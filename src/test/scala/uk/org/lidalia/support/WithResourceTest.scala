package uk.org.lidalia.support

import org.scalatest._
import uk.org.lidalia.lang.ResourceFactory

trait WithResourceTest extends FunSuiteLike {

  def test[A](testName: String, factory: ResourceFactory[A], testTags: Tag*)(testFun: (A) => Unit) {
    test(testName, testTags:_*) {
      factory.withA { resource =>
        testFun(resource)
      }
    }
  }
}