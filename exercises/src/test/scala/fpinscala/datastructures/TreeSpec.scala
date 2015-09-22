package fpinscala.datastructures

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tree._;

/**
 * Refer to the following matcher examples:
 * http://www.scalatest.org/user_guide/matchers_quick_reference
 */
@RunWith(classOf[JUnitRunner])
class TreeSpec extends FlatSpec with Matchers {
  
  trait TestSets {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val leaf4 = Leaf(4)
    val leaf5 = Leaf(5)
    
    val branchA = Branch(leaf1, leaf2)
    val branchC = Branch(leaf3, leaf4)
    val branchB = Branch(branchC, leaf5)
    
    val trunk = Branch(branchA, branchB)
  }  
  
  "treeSize" should "count nodes" in {
    new TestSets {    
      treeSize(trunk) shouldBe 9
    }
  }

  "maximum" should "return the largest value" in {
    new TestSets {    
      maximum(trunk) shouldBe 5
    }
  }

  "depth" should "return the largest path length" in {
    new TestSets {    
      depth(trunk) shouldBe 3
    }
  }

  "map" should "apply a function to element in a tree" in {
    new TestSets {    
      treeSize(map(trunk)(_ + 1)) shouldBe 9
      maximum(map(trunk)(_ + 1)) shouldBe 6
      depth(map(trunk)(_ + 1)) shouldBe 3
    }
  }
  
  "fold" should "enable factoring of existing methods" in {
    new TestSets {    
      treeSize329(trunk) shouldBe 9   
      maximum329(trunk) shouldBe 5 
      depth329(trunk) shouldBe 3

      treeSize329(map329(trunk)(_ + 1)) shouldBe 9
      maximum329(map329(trunk)(_ + 1)) shouldBe 6
      depth329(map329(trunk)(_ + 1)) shouldBe 3
    }
  }  
}
