package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    //rng => {
    //  val (a, rng2) = s(rng)
    //  (f(a), rng2)
    //}
      
    // Exercise 6.9:
    flatMap(s)(a => unit(f(a)))
  }

  // Exercise 6.1:
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    if (i > 0)
      (i, rng1)
    else if (i > Int.MinValue)
      (-i, rng1)
    else
      (0, rng1)
  } 

  // Exercise 6.2:
  def double(rng: RNG): (Double, RNG) = {
    //val (i, rng1) = nonNegativeInt(rng)
    //(i / (Int.MaxValue.toDouble + 1), rng1)
    
    // Exercise 6.5:
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
  } 

  // Exercise 6.3:
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    //val (i, rng1) = rng.nextInt
    //val (d, rng2) = double(rng1)
    //((i, d), rng2)
    
    //map2(int, double)((a, b) => (a, b))(rng)
    
    // Exercise 6.6:
    both(int, double)(rng)    
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    //val ((i, d), rng1) = intDouble(rng)
    //((d, i), rng1)

    //map(intDouble){case (a, b) => (b, a)}(rng)
    
    //map2(double, int)((a, b) => (a, b))(rng)
    
    // Exercise 6.6:
    both(double, int)(rng)    
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 6.4:
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    //def go(count: Int)(rng: RNG, acc: List[Int]): (List[Int], RNG) = {
    //  if (count < 1) {
    //    (acc, rng)
    //  } else {
    //    val (i, rng1) = rng.nextInt
    //    go(count - 1)(rng1, i :: acc)
    //  }
    //}
    //
    //go(count)(rng, List[Int]())
    
    // Exercise 6.7:
    sequence(List.fill(count)(int))(rng)
  }
  
  // Exercise 6.6:
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //rng => {
    //  val (a, rng1) = ra(rng)
    //  val (b, rng2) = rb(rng1)
    //  (f(a, b), rng2)
    //}
    
    // Exercise 6.9:
    flatMap(ra)(a => map(rb)(b => f(a, b)))    
  }
  
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  // Exercise 6.7:
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(fs: List[Rand[A]], rlist: Rand[List[A]]): Rand[List[A]] = {
        if (fs == Nil)
          rlist
        else
          go(fs.tail, map2(fs.head, rlist)((a, list) => a :: list))
    }
    
    // The last A is added to the head of the list, but we want it at the end:
    val rlist = go(fs, unit(List[A]()))
    map(rlist)(list => list.reverse)
  }
  
  // Exercise 6.8:
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      val (b, rng2) = g(a)(rng1)
      (b, rng2)
    }
  }
  
  def nonNegatgiveLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i => 
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegatgiveLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
