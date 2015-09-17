package fpinscala.gettingstarted

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MyModule._;
import PolymorphicFunctions._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class GettingStartedSpec extends FlatSpec with Matchers {
  
  "fibs" should "be correct for the first two values" in {
    fib(0) shouldBe 0
    fib(1) shouldBe 1
  }
  
  it should "be correct for the next four values" in {
    fib(2) shouldBe 1
    fib(3) shouldBe 2
    fib(4) shouldBe 3
    fib(5) shouldBe 5
  }

  val asc = (x: Int, y: Int) => x <= y
  
  "isSorted" should "be true for sorted arrays" in {
    isSorted(Array(1), asc) shouldBe true
    isSorted(Array(1, 2), asc) shouldBe true
    isSorted(Array(1, 2, 3), asc) shouldBe true
    isSorted(Array(1, 1, 3), asc) shouldBe true
  }
  
  it should "be false for unsorted arrays" in {
    isSorted(Array(3, 2, 1), asc) shouldBe false
    isSorted(Array(1, 3, 2), asc) shouldBe false
  }
  
  "curry" should "work" in {
    val sum = (a: Int, b: Int) => a + b
    curry(sum)(1)(2) shouldBe 3    
  }
  
  "uncurry" should "work" in {
    def sum(a: Int)(b: Int) = a + b
    uncurry(sum)(1, 2)    
  }
  
  "compose" should "work" in {
    def square(a: Int) = a * a
    def half(a: Int) = a / 2
    compose(square, half)(10)   
  }
  
}
