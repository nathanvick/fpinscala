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
    
    val abc = Cons("A", Cons("B", Cons("C", Nil)))    
    val cba = Cons("C", Cons("B", Cons("A", Nil)))
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

  "foldLeft" should "fold from left to right" in {
    new TestSets {
      foldLeft(Nil: List[Int], 0)(_ + _) shouldBe 0
      foldLeft(abc, Nil: List[String])((b, a) => Cons(a, b)) shouldBe cba
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

  "foldLeft313" should "fold from left to right" in {
    new TestSets {
      foldLeft313(Nil: List[Int], 0)(_ + _) shouldBe 0
      foldLeft313(abc, Nil: List[String])((b, a) => Cons(a, b)) shouldBe cba
    }
  }

  "foldRight313" should "fold from right to left" in {
    new TestSets {
      foldRight313(Nil: List[Int], 0)(_ + _) shouldBe 0
      foldRight(abc, Nil: List[String])((a, b) => Cons(a, b)) shouldBe abc
    }
  }
  
  "append314" should "concatenate two lists" in {
    new TestSets {
      append314(Nil, abc) shouldBe abc
      append314(abc, Nil) shouldBe abc
      append314(Cons("A", Nil), Cons("B", Cons("C", Nil))) shouldBe abc
    }
  }
  
  "flatten" should "flatten a list of lists" in {
    new TestSets {
      flatten(Nil) shouldBe Nil
      
      val innerList1 = Cons("1.1", Cons("1.2", Cons("1.3", Nil)))
      val innerList2 = Cons("2.1", Cons("2.2", Cons("2.3", Nil)))
      
      val listOfLists: List[List[String]] = 
          Cons(innerList1, Cons(innerList2, Nil))
          
      val flattened: List[String] = 
          Cons("1.1", Cons("1.2", Cons("1.3", Cons("2.1", Cons("2.2", Cons("2.3", Nil)))))) 
          
      flatten(listOfLists) shouldBe flattened
    }
  }    

  "addOne" should "increment each element" in {
    new TestSets {
      addOne(Nil) shouldBe Nil
      addOne(l3) shouldBe Cons(4, Cons(3, Cons(2, Nil)))      
    }
  }
  
  "stringify" should "convert each element to a String" in {
    new TestSets {
      stringify(Nil) shouldBe Nil
      stringify(Cons(3.0, Cons(2.0, Cons(1.0, Nil)))) shouldBe Cons("3.0", Cons("2.0", Cons("1.0", Nil)))      
    }
  }
  
  "map" should "return a transformed list" in {
    new TestSets {
      map(Nil)(x => x) shouldBe Nil
      map(l3)(_ + 1) shouldBe Cons(4, Cons(3, Cons(2, Nil)))      
      map(Cons(3.0, Cons(2.0, Cons(1.0, Nil))))(_.toString) shouldBe Cons("3.0", Cons("2.0", Cons("1.0", Nil)))      
    }
  }
  
  "filter" should "remove odd numbers, given a function that tests if a number if even" in {
    new TestSets {
      val isEven = (x: Int) => x % 2 == 0
      
      filter(Nil)(isEven) shouldBe Nil
      filter(l3)(isEven) shouldBe Cons(2, Nil)
    }
  }
  
  "flatMap" should "return a transformed list" in {
    new TestSets {
      flatMap(Nil)(x => Cons(x, Nil)) shouldBe Nil
      flatMap(l3)(x => Cons(x + 1, Nil)) shouldBe Cons(4, Cons(3, Cons(2, Nil)))      
      flatMap(Cons(3.0, Cons(2.0, Cons(1.0, Nil))))(x => Cons(x.toString, Nil)) shouldBe Cons("3.0", Cons("2.0", Cons("1.0", Nil)))      
    }
  }

  "filter321" should "remove odd numbers, given a function that tests if a number if even" in {
    new TestSets {
      val isEven = (x: Int) => x % 2 == 0
      
      filter321(Nil)(isEven) shouldBe Nil
      filter321(l3)(isEven) shouldBe Cons(2, Nil)
    }
  }

  "add" should "return the sum of respective elements in two lists" in {
    new TestSets {
      add(Nil, Nil) shouldBe Nil
      
      // One Nil
      add(Nil, l1) shouldBe Nil
      add(l1, Nil) shouldBe Nil
      
      // One less
      add(l1, l2) shouldBe Cons(3, Nil)
      add(l2, l1) shouldBe Cons(3, Nil)
      
      add(l3, l3) shouldBe Cons(6, Cons(4, Cons(2, Nil)))
    }
  }
  
  "zipWith" should "execute the specified function over respective elements in two lists, and return the result" in {
    val f = (a: Int, b: Int) => a + b
    
    new TestSets {
      zipWith(Nil, Nil)(f) shouldBe Nil
      
      // One Nil
      zipWith(Nil, l1)(f) shouldBe Nil
      zipWith(l1, Nil)(f) shouldBe Nil
      
      // One less
      zipWith(l1, l2)(f) shouldBe Cons(3, Nil)
      zipWith(l2, l1)(f) shouldBe Cons(3, Nil)
      
      zipWith(l3, l3)(f) shouldBe Cons(6, Cons(4, Cons(2, Nil)))
    }
  }  
  
  "hasSubsequence" should "evaulate correctly" in {
    val f = (a: Int, b: Int) => a + b
    
    new TestSets {
      hasSubsequence(Nil, Nil) shouldBe false
      hasSubsequence(Nil, l1) shouldBe false
      hasSubsequence(l1, Nil) shouldBe false

      hasSubsequence(l3, l3) shouldBe true
      hasSubsequence(l3, l2) shouldBe true
      hasSubsequence(l3, l1) shouldBe true
      
      hasSubsequence(l3, Cons(3, Nil)) shouldBe true
      hasSubsequence(l3, Cons(3, Cons(2, Nil))) shouldBe true

      hasSubsequence(l3, Cons(2, Nil)) shouldBe true
      
      hasSubsequence(l2, l3) shouldBe false
      hasSubsequence(l1, l2) shouldBe false
      hasSubsequence(l1, l3) shouldBe false      
    }
  }   
  
}
