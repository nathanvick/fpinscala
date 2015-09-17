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

  "tail of Nil" should "fail" in {
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

  "setHead of Nil" should "fail" in {
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

  "drop of too many values" should "fail" in {
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
  
  "drop " should " not be negative" in {
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
  
  "dropWhile with a check-for-odd function " should "discard odd prefix" in {
    new TestSets {
      dropWhile(l1, (a: Int) => a % 2 == 1) shouldBe Nil
      dropWhile(l2, (a: Int) => a % 2 == 1) shouldBe l2
      dropWhile(l3, (a: Int) => a % 2 == 1) shouldBe l2
    }
  }
  
  "dropWhile with a constant false function" should "not discard anything" in {
    new TestSets {
      dropWhile(Nil, (a: Int) => false) shouldBe Nil
      dropWhile(l1, (a: Int) => false) shouldBe l1
      dropWhile(l2, (a: Int) => false) shouldBe l2
      dropWhile(l3, (a: Int) => false) shouldBe l3
    }
  }

  "dropWhile with a constant true function" should "return Nil" in {
    new TestSets {    
      dropWhile(Nil, (a: Int) => true) shouldBe Nil
      dropWhile(l1, (a: Int) => true) shouldBe Nil
      dropWhile(l2, (a: Int) => true) shouldBe Nil
      dropWhile(l3, (a: Int) => true) shouldBe Nil
    }
  }  
  
}
