package fpinscala.state

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import RNG._;
import State._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class StateSpec extends FlatSpec with Matchers {

  // Exercise 6.1:
  "nonNegativeInt" should "return an integer from 0 to Integer.MaxValue, inclusive" in {
    nonNegativeInt(Simple(-5581))._1 shouldBe 2147283612
    nonNegativeInt(Simple(42))._1 shouldBe 16159453
  }
  
  // Exercise 6.2:
  "double" should "return a double" in {
    double(Simple(-5581))._1 shouldBe 0.9999068509787321
    double(Simple(42))._1 shouldBe 0.007524831686168909
  }

  // Exercise 6.3:
  "double3, doubleInt, double3" should "return expected values" in {
    intDouble(Simple(-5581))._1 shouldBe (-2147283612,0.9395831124857068)
    doubleInt(Simple(-5581))._1 shouldBe (0.9999068509787321,2017739370)
    double3(Simple(-5581))._1 shouldBe (0.9999068509787321,0.9395831124857068,0.5378132513724267)
  }  
  
  // Exercise 6.4:
  "ints" should "return a list of ints" in {
    ints(3)(Simple(-5581))._1 shouldBe List(1154945163, 2017739370, -2147283612)
    ints(3)(Simple(42))._1 shouldBe List(-340305902, -1281479697, 16159453)
  }  

  // Exercise 6.8:
  "nonNegatgiveLessThan" should "return an int in the specified range" in {
    nonNegatgiveLessThan(10)(Simple(-5581))._1 shouldBe 2
    nonNegatgiveLessThan(10)(Simple(42))._1 shouldBe 3
  }
  
}
