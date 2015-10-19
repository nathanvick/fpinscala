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

  trait TestStreams {
    val l1 = cons(1, Empty)
    val l2 = cons(2, l1)
    val l3 = cons(3, l2)
    
    val abc = cons("A", cons("B", cons("C", Empty)))    
    val cba = cons("C", cons("B", cons("A", Empty)))
  } 

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
    constant(42).take(3).toList shouldBe List(42, 42, 42)
  }

  // Exercise 5.9:
  "from" should "return an infinite stream that increments by one" in {
    from(1).take(3).toList shouldBe List(1, 2, 3)
  }
  
  // Exercise 5.10:
  "fibs" should "return an infinite stream of fibonacci numbers" in {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }
   
  // Exercise 5.11:
  "unfold" should "be able to return an infinite stream " in {
    unfold(1)(a => Some(a, a + 1)).take(3).toList shouldBe List(1, 2, 3)
  }  

  // Exercise 5.11:
  it should "be able to return a finite stream " in {
    unfold(1)(a => if (a < 3) Some(a, a + 1) else None).take(3).toList shouldBe List(1, 2)
  }

  // Exercise 5.13:
  "zipWith" should "be able to return a finite stream " in {
    constant(1).zipWith(constant(2).take(4))(_ + _).toList shouldBe List(3, 3, 3, 3)
  }

  "zipAll" should "be able to return a finite stream " in {
    constant(1).zipAll(constant(2).take(1)).take(2).toList shouldBe List((Some(1), Some(2)), (Some(1), None))
  }

  // Exercise 5.14:
  "startsWith" should "return true, given a substream" in {
    from(1).startsWith(Stream(1, 2, 3)) shouldBe true
  } 

  it should "return false otherwise" in {
    from(1).startsWith(Stream(1, 2, 4)) shouldBe false
  } 
 
  "startsWith" should "evaulate correctly" in {
    new TestStreams {
      Empty.startsWith(Empty) shouldBe true
      Empty.startsWith(l1) shouldBe false
      l1.startsWith(Empty) shouldBe true

      l3.startsWith(l3) shouldBe true
      l3.startsWith(l2) shouldBe false
      l3.startsWith(l1) shouldBe false
      
      l3.startsWith(cons(3, Empty)) shouldBe true
      l3.startsWith(cons(3, cons(2, Empty))) shouldBe true

      l3.startsWith(cons(2, Empty)) shouldBe false
      
      l2.startsWith(l3) shouldBe false
      l1.startsWith(l2) shouldBe false
      l1.startsWith(l3) shouldBe false      
    }
  }

  "hasSubsequence" should "evaulate correctly" in {
    new TestStreams {
      Empty.hasSubsequence(Empty) shouldBe true
      Empty.hasSubsequence(l1) shouldBe false
      l1.hasSubsequence(Empty) shouldBe true

      l3.hasSubsequence(l3) shouldBe true
      l3.hasSubsequence(l2) shouldBe true
      l3.hasSubsequence(l1) shouldBe true
      
      l3.hasSubsequence(cons(3, Empty)) shouldBe true
      l3.hasSubsequence(cons(3, cons(2, Empty))) shouldBe true

      l3.hasSubsequence(cons(2, Empty)) shouldBe true
      
      l2.hasSubsequence(l3) shouldBe false
      l1.hasSubsequence(l2) shouldBe false
      l1.hasSubsequence(l3) shouldBe false      
    }
  }

}
