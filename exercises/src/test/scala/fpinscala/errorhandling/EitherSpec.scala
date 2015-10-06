package fpinscala.errorhandling

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Either._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class EitherSpec extends FlatSpec with Matchers {
  
  trait TestSets {
  }  
  
  "EitherSpec" should "have tests" in {
    new TestSets {    
      1 shouldBe 1
    }
  }
}
