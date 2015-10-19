package fpinscala.laziness

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Stream._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class StreamSpec extends FlatSpec with Matchers {
  
  // Exercise 5.1:
  "toList" should "convert a Stream to a List" in {
    Stream("a", "b", "c").toList shouldBe List("a", "b", "c")
  }
  
  // Exercise 5.2:
  "take" should "return first n elements in a new stream" in {
    Stream("a", "b", "c").take(2).toList shouldBe List("a", "b")
  }
  
  // Exercise 5.2:
  "drop" should "return all but the first n elements in a new stream" in {
    Stream("a", "b", "c").drop(1).toList shouldBe List("b", "c")
  }
  
  // Exercise 5.3/5.5:
  "takeWhile" should "return the longest prefix where all elements satisfiy the predicate" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

  // Exercise 5.4:
  "forAll" should "return false if any element does not match the predicate" in {
    Stream(1, 2, 3).forAll(_ == 1) shouldBe false
  }

  it should "return true if all elements match the predicate" in {
    Stream(1, 1, 1).forAll(_ == 1) shouldBe true
  }

  it should "return true for an empty stream" in {
    Stream().forAll(_ == 1) shouldBe true
  }

  // Exercise 5.6:
  "forAll" should "return the first element" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
  }

  it should "return None for an empty stream" in {
    Stream().headOption shouldBe None
  }

  // Exercise 5.7:
  "map" should "return a new stream with f applied to each element" in {
    Stream(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
  }

  "filter" should "return a new stream with elements that match the predicate" in {
    Stream(1, 2, 3).filter(_ % 2 == 0).toList shouldBe List(2)
  }

  "append" should "return a new stream the specified element at the end" in {
    Stream(1, 2, 3).append(4).toList shouldBe List(1, 2, 3, 4)
  }

  "flatMap" should "return a new stream with f applied to each element" in {
    Stream(1, 2, 3).flatMap(x => Stream(x * 2)).toList shouldBe List(2, 4, 6)
  }  

  // Exercise 5.8:
  "constant" should "return an infinite stream containing a constant value" in {
    Stream.constant(42).take(3).toList shouldBe List(42, 42, 42)
  }

  // Exercise 5.9:
  "from" should "return an infinite stream that increments by one" in {
    Stream.from(1).take(3).toList shouldBe List(1, 2, 3)
  }
  
  // Exercise 5.10:
  "fibs" should "return an infinite stream of fibonacci numbers" in {
    Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }
   
  // Exercise 5.11:
  "unfold" should "be able to return an infinite stream " in {
    Stream.unfold(1)(a => Some(a, a + 1)).take(3).toList shouldBe List(1, 2, 3)
  }  

  // Exercise 5.11:
  it should "be able to return a finite stream " in {
    Stream.unfold(1)(a => if (a < 3) Some(a, a + 1) else None).take(3).toList shouldBe List(1, 2)
  }  
  
}
