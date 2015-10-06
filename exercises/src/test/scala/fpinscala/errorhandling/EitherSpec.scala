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
  
  // Exercise 4.6:
  "map" should "should apply a function to the right value" in {
    Right(1) map (_ + 1) shouldBe Right(2)
  }

  it should "should not apply a function to the left value" in {
    Left("error") map ((x: Int) => x + 1) shouldBe Left("error")
  }

  "flatMap" should "should apply a function to the right value" in {
    Right(1) flatMap (x => Right(x + 1)) shouldBe Right(2)
  }

  it should "should fail if the function fails" in {
    Right(1) flatMap (x => Left("error")) shouldBe Left("error")
  }

  it should "should not apply a function to the left value" in {
    Left("error") flatMap ((x: Int) => Right(x + 1)) shouldBe Left("error")
  }

  "orElse" should "should preserve a right value" in {
    Right(1) orElse (Right(2)) shouldBe Right(1)
    Right(1) orElse (Left("error")) shouldBe Right(1)
  }

  it should "should replace a left value" in {
    Left("error") orElse (Right(2)) shouldBe Right(2)
    Left("error") orElse (Left("error2")) shouldBe Left("error2")
  }

  "map2" should "should apply a function to the right value" in {
    Right(1).map2(Right(2))(_ + _) shouldBe Right(3)
  }

  it should "should return a left value, if applied to a left value" in {
    Right(1).map2 (Left("error2"))((x, y) => x.toString + y.toString) shouldBe Left("error2")
  }  

  it should "should return a left value, if given a left value as an argument" in {
    Left("error1").map2 (Right(2))((x, y) => x.toString + y.toString) shouldBe Left("error1")
  }  

  it should "should return the first left value encountered" in {
    Left("error1").map2 (Left("error2"))((x, y) => x.toString + y.toString) shouldBe Left("error1")
  }

  // Exercise 4.7:
  "sequence" should "should combine two right values" in {
    sequence(Right(1) :: Right(2) :: Nil) shouldBe Right(1 :: 2 :: Nil)
  }
  
  it should "should return the left value if the first value is a left value" in {
    sequence(Left("error") :: Right(2) :: Nil) shouldBe Left("error")
  }
  
  it should "should return the left value if the second value is a left value" in {
    sequence(Right(1) :: Left("error") :: Nil) shouldBe Left("error")
  }  

  it should "should return the first error if all values are errors" in {
    //sequence(Left("error1") :: Left("error2") :: Nil) shouldBe Left("error1")
    sequence(Left("error1") :: Left("error2") :: Nil) shouldBe Left("error1")
  }  

  it should "should return Nil if the list of either values is empty" in {
    sequence(Nil) shouldBe Right(Nil)
  }

  "traverse" should "should return a list of values, in an right either" in {
    traverse(1 :: 2 :: Nil)(x => Right(x)) shouldBe Right(1 :: 2 :: Nil)
  }
  
  it should "should return the left value if the first value is a left value" in {
    traverse(1 :: 2 :: Nil)(x => if (x % 2 == 0) Right(x) else Left("error" + x)) shouldBe Left("error1")
  }
  
  it should "should return the a left value if all values are left values" in {
    traverse(1 :: 2 :: Nil)(x => Left("error" + x)) shouldBe Left("error1")
  }  

}
