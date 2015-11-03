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
    ints(3)(Simple(-5581))._1 shouldBe List(int(Simple(-5581))._1, int(int(Simple(-5581))._2)._1, int(int(int(Simple(-5581))._2)._2)._1)
    ints(3)(Simple(-5581))._1 shouldBe List(-2147283612, 2017739370, 1154945163)
    ints(3)(Simple(42))._1 shouldBe List(16159453, -1281479697, -340305902)
  }
  
  // Exercise 6.6:
  "both" should "execute two state transformations in sequence" in {
    both(int, int)(Simple(-5581))._1 shouldBe (-2147283612, 2017739370)
    both(int, int)(Simple(42))._1 shouldBe (16159453, -1281479697)

    both(int, nonNegativeIntRand)(Simple(-5581))._1 shouldBe (int(Simple(-5581))._1, nonNegativeIntRand(int(Simple(-5581))._2)._1)
    both(int, nonNegativeIntRand)(Simple(42))._1 shouldBe (int(Simple(42))._1, nonNegativeIntRand(int(Simple(42))._2)._1)
    
    both(int, nonNegativeIntRand)(Simple(-5581))._1 shouldBe (-2147283612, 2017739370)
    both(int, nonNegativeIntRand)(Simple(42))._1 shouldBe (16159453, 1281479697)
  }
  
  // Exercise 6.7:
  "sequence" should "execute all state transformations in sequence" in {
    RNG.sequence(List(int, nonNegativeIntRand))(Simple(-5581))._1 shouldBe List(-2147283612, 2017739370)
    RNG.sequence(List(int, nonNegativeIntRand))(Simple(42))._1 shouldBe List(16159453, 1281479697)
  }

  // Exercise 6.8:
  "nonNegatgiveLessThan" should "return an int in the specified range" in {
    nonNegatgiveLessThan(10)(Simple(-5581))._1 shouldBe 2
    nonNegatgiveLessThan(10)(Simple(42))._1 shouldBe 3
  }

  // Exercise 6.10 (extra mile):
  "_nonNegativeInt" should "return an integer from 0 to Integer.MaxValue, inclusive" in {
    _nonNegativeInt.run(Simple(-5581))._1 shouldBe 2147283612
    _nonNegativeInt.run(Simple(42))._1 shouldBe 16159453
  }

  "_ints" should "return a list of ints" in {
    _ints(3).run(Simple(-5581))._1 shouldBe List(-2147283612, 2017739370, 1154945163)
    _ints(3).run(Simple(42))._1 shouldBe List(16159453, -1281479697, -340305902)
  }  

  "simulateMachine" should "be unaffected by empty input" in {
    val ((coins, candies), machine) = simulateMachine(List()).run(Machine(true, 7, 0))
    coins shouldBe 0
    candies shouldBe 7
    machine.locked shouldBe true
  }
  
  it should "unlock when it receives a coin" in {
    val ((coins, candies), machine) = simulateMachine(List(Coin)).run(Machine(true, 7, 0))
    coins shouldBe 1
    candies shouldBe 7
    machine.locked shouldBe false
  }
  
  it should "stay unlocked when it receives a coin" in {
    val ((coins, candies), machine) = simulateMachine(List(Coin)).run(Machine(false, 7, 0))
    coins shouldBe 1
    candies shouldBe 7
    machine.locked shouldBe false
  }

  it should "lock when it dispenses a candy" in {
    val ((coins, candies), machine) = simulateMachine(List(Turn)).run(Machine(false, 7, 1))
    coins shouldBe 1
    candies shouldBe 6
    machine.locked shouldBe true
  }

  it should "ignore turns when it is locked" in {
    val ((coins, candies), machine) = simulateMachine(List(Turn)).run(Machine(true, 7, 0))
    coins shouldBe 0
    candies shouldBe 7
    machine.locked shouldBe true
  }
  
  it should "ignore turns when it is out of candy and locked" in {
    val ((coins, candies), machine) = simulateMachine(List(Turn)).run(Machine(true, 0, 99))
    coins shouldBe 99
    candies shouldBe 0
    machine.locked shouldBe true
  }

  it should "ignore turns when it is out of candy and unlocked" in {
    val ((coins, candies), machine) = simulateMachine(List(Turn)).run(Machine(false, 0, 99))
    coins shouldBe 99
    candies shouldBe 0
    machine.locked shouldBe false
  }
  
  it should "ignore coins when it is out of candy and locked" in {
    val ((coins, candies), machine) = simulateMachine(List(Coin)).run(Machine(true, 0, 99))
    coins shouldBe 99
    candies shouldBe 0
    machine.locked shouldBe true
  }

  it should "ignore coins when it is out of candy and unlocked" in {
    val ((coins, candies), machine) = simulateMachine(List(Coin)).run(Machine(false, 0, 99))
    coins shouldBe 99
    candies shouldBe 0
    machine.locked shouldBe false
  }
  
}
