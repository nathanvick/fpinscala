package fpinscala.datastructures

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import List._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class ListSpec extends FlatSpec with Matchers {
  
  trait TestSets {
    val l1 = Cons(1, Nil)
    val l2 = Cons(2, l1)
    val l3 = Cons(3, l2)
  }  
  
  "tail" should "be remaining values" in {
    new TestSets {    
      tail(Cons(13, Nil)) shouldBe Nil
  
      tail(Cons(13, l1)) shouldBe l1
  
      tail(Cons(13, l2)) shouldBe l2
    }
  }

  it should "fail, given Nil" in {
    intercept[UnsupportedOperationException] {
      tail(Nil)
    }
  }
  
  "setHead" should "replace head and include remaining values" in {
    new TestSets {    
      setHead(Cons(13, Nil), 31) shouldBe Cons(31, Nil)
  
      setHead(Cons(13, l1), 31) shouldBe Cons(31, l1)
  
      setHead(Cons(13, l2), 31) shouldBe Cons(31, l2)
    }
  }

  it should "fail, given Nil" in {
    intercept[UnsupportedOperationException] {
      setHead(Nil, 13)
    }
  }
  
  "drop" should "discard specified number of values" in {
    new TestSets {
      drop(l3, 0) shouldBe l3
      drop(l3, 1) shouldBe l2
      drop(l3, 2) shouldBe l1
      drop(l3, 3) shouldBe Nil
      
      // It's fine with me to drop nothing from Nil
      drop(Nil, 0) shouldBe Nil      
    }
  }

  it should "fail, given too many values" in {
    new TestSets {    
      intercept[UnsupportedOperationException] {
        drop(Nil, 1)
      }
  
      intercept[UnsupportedOperationException] {
        drop(l1, 2)
      }
  
      intercept[UnsupportedOperationException] {
        drop(l2, 3)
      }
  
      intercept[UnsupportedOperationException] {
        drop(l3, 4)
      }
    }
  }
  
  it should "not be negative" in {
    new TestSets {    
      intercept[IllegalArgumentException] {
        drop(Nil, -1)
      }
  
      intercept[IllegalArgumentException] {
        drop(l1, -1)
      }
  
      intercept[IllegalArgumentException] {
        drop(l2, -2)
      }
  
      intercept[IllegalArgumentException] {
        drop(l3, -3)
      }
    }
  }
  
  "dropWhile" should "discard odd prefix, given a check-for-odd function" in {
    new TestSets {
      dropWhile(l1, (a: Int) => a % 2 == 1) shouldBe Nil
      dropWhile(l2, (a: Int) => a % 2 == 1) shouldBe l2
      dropWhile(l3, (a: Int) => a % 2 == 1) shouldBe l2
    }
  }
  
  it should "not discard anything, given a constant false function" in {
    new TestSets {
      dropWhile(Nil, (a: Int) => false) shouldBe Nil
      dropWhile(l1, (a: Int) => false) shouldBe l1
      dropWhile(l2, (a: Int) => false) shouldBe l2
      dropWhile(l3, (a: Int) => false) shouldBe l3
    }
  }

  it should "return Nil, given a constant true function" in {
    new TestSets {    
      dropWhile(Nil, (a: Int) => true) shouldBe Nil
      dropWhile(l1, (a: Int) => true) shouldBe Nil
      dropWhile(l2, (a: Int) => true) shouldBe Nil
      dropWhile(l3, (a: Int) => true) shouldBe Nil
    }
  }

  "init" should "discard the last element" in {
    new TestSets {
      init(l1) shouldBe Nil
      init(l2) shouldBe Cons(2, Nil)
      init(l3) shouldBe Cons(3, Cons(2, Nil))
    }
  }

  it should "fail on Nil" in {
    new TestSets {
      intercept[UnsupportedOperationException] {
        init(Nil)
      }
    }
  }

  "length" should "count elements of a list" in {
    new TestSets {
      List.length(Nil) shouldBe 0
      List.length(l1) shouldBe 1
      List.length(l2) shouldBe 2
      List.length(l3) shouldBe 3
    }
  }

  "foldLeft" should "work" in {
    new TestSets {
      foldLeft(Nil: List[Int], 0)(_ + _) shouldBe 0
      foldLeft(l1, 0)(_ + _) shouldBe 1
      foldLeft(l2, 0)(_ + _) shouldBe 3
      foldLeft(l3, 0)(_ + _) shouldBe 6
    }
  }

  "sum310" should "sum elements of a list" in {
    new TestSets {
      sum310(Nil) shouldBe 0
      sum310(l1) shouldBe 1
      sum310(l2) shouldBe 3
      sum310(l3) shouldBe 6
    }
  }

  "product310" should "multiple elements of a list" in {
    val l1 = Cons(1.0, Nil)
    val l2 = Cons(2.0, l1)
    val l3 = Cons(3.0, l2)

    product310(Nil) shouldBe 1.0
    product310(l1) shouldBe 1.0
    product310(l2) shouldBe 2.0
    product310(l3) shouldBe 6.0
  }

  "length310" should "count elements of a list" in {
    new TestSets {
      length310(Nil) shouldBe 0
      length310(l1) shouldBe 1
      length310(l2) shouldBe 2
      length310(l3) shouldBe 3
    }
  }

  "reverse" should "return elements in the opposite order" in {
    new TestSets {
      reverse(Nil) shouldBe Nil
      reverse(l1) shouldBe l1
      reverse(l2) shouldBe Cons(1, Cons(2, Nil))
      reverse(l3) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

}
