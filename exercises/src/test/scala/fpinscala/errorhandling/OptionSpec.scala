package fpinscala.errorhandling

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import Option._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class OptionSpec extends FlatSpec with Matchers {
  
  // Exercise 4.1:
  "map" should "should apply a function on Some" in {
    Some(1) map(_ + 1) shouldBe Some(2)
  }
  
  it should "should do nothing on None" in {
    None map(_.toString()) shouldBe None
  }
  
  "getOrElse" should "should return the contained value on Some" in {
    Some(1).getOrElse(2) shouldBe 1
  }
  
  it should "should return the default value on None" in {
    None.getOrElse(2) shouldBe 2
  }
  
  "flatMap" should "should apply a function on Some" in {
    Some(1) flatMap(x => Some(x + 1)) shouldBe Some(2)
  }
  
  it should "should do nothing on None" in {
    None flatMap(x => Some(x.toString())) shouldBe None
  }  
  
  "orElse" should "should return itself on Some" in {
    Some(1).orElse(Some(2)) shouldBe Some(1)
  }
  
  it should "should return the default value on None" in {
    None.orElse(Some(2)) shouldBe Some(2)
  }
  
  "filter" should "should keep if the function returns true" in {
    Some(1) filter(_ == 1) shouldBe Some(1)
  }
  
  it should "should throw out a value if the function returns false" in {
    Some(1) filter(_ == 2) shouldBe None
  }
  
  it should "should do nothing on None" in {
    None filter(x => true) shouldBe None
  }
  
  // Exercise 4.2:
  "variance" should "should return the average squared distance from the mean" in {
    variance(Seq(2.0, 6.0)) shouldBe Some(4.0)
  }
  
  it should "should return Some(0.0) for a one-element Seq" in {
    variance(Seq(1.0)) shouldBe Some(0.0)
  }
  
  it should "should return None for an empty Seq" in {
    variance(Seq()) shouldBe None
  }

  // Exercise 4.3:
  "map2" should "should combine two non-empty options" in {
    map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
  }
  
  it should "should return None if the first Option is empty" in {
    map2(Some(1), None)(_ + _) shouldBe None
  }
  
  it should "should return None if the second Option is empty" in {
    map2(None, Some(2))((a, b) => (a, b)) shouldBe None
  }  

  it should "should return None if both Options are empty" in {
    map2(None, None)((a, b) => (a, b)) shouldBe None
  }

  // Exercise 4.4:
  "sequence" should "should combine two non-empty options" in {
    sequence(Some(1) :: Some(2) :: Nil) shouldBe Some(1 :: 2 :: Nil)
  }
  
  it should "should return None if the first Option is empty" in {
    sequence(Some(1) :: None :: Nil) shouldBe None
  }
  
  it should "should return None if the second Option is empty" in {
    sequence(None :: Some(2) :: Nil) shouldBe None
  }  

  it should "should return None if all Options are empty" in {
    sequence(None :: None :: Nil) shouldBe None
  }  

  it should "should return Nil if the list of Options is empty" in {
    sequence(Nil) shouldBe Some(Nil)
  }

  // Exercise 4.5:
  "traverse" should "should return a list of values, in an Option" in {
    traverse(1 :: 2 :: Nil)(x => Some(x)) shouldBe Some(1 :: 2 :: Nil)
  }
  
  it should "should return None if the first Option is empty" in {
    traverse(1 :: 2 :: Nil)(x => if (x % 2 == 0) Some(x) else None) shouldBe None
  }
  
  it should "should return None if all Options are empty" in {
    traverse(1 :: 2 :: Nil)(x => None) shouldBe None
  }  
}
