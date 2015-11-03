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
    //val (i, rng1) = rng.nextInt
    //if (i < 0) (-i, rng1) else (i, rng1)
    
    map(int)(i => if (i < 0) -i else i)(rng)
  } 

  def nonNegativeIntRand: Rand[Int] = {
    map(int)(i => if (i < 0) -i else i)
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
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
    
    // Exercise 6.9:
    //flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
  
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  // Exercise 6.7:
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //def go(fs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = {
    //    if (fs == Nil)
    //      acc
    //    else
    //      go(fs.tail, map2(acc, fs.head)((list, a) => a :: list))
    //}    
    //val acc = go(fs, unit(List[A]()))
    //map(acc)(list => list.reverse)
    
    //val acc = fs.foldLeft(unit(List[A]()))((acc, ra) => map2(acc, ra)((list, a) => a :: list))
    //map(acc)(list => list.reverse)    
    
    fs.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)(_ :: _))
  }
  
  // Exercise 6.8:
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
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
  // Exercise 6.10:
  def map[B](f: A => B): State[S, B] = {
    //State(s => {
    //  val (a, s1) = run(s)
    //  (f(a), s1)
    //})
    
    flatMap(a => State.unit((f(a))))
  }
  
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    //State(s => {
    //  val (a, s1) = run(s)
    //  val (b, s2) = sb.run(s1)
    //  (f(a, b), s2)
    //})
    
    //flatMap(a => sb.map(b => f(a, b)))
    
    for (a <- this; b <- sb) yield (f(a, b))
  }
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  
  // Exercise 6.11:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    if (inputs.isEmpty) {
      State(m => extractState(m))
    } else {
      val stateTransitions = inputs.map(evaluateIntput)
      sequence(stateTransitions).map(results => results.last)
    }
  }

  def evaluateIntput(in: Input): State[Machine, (Int, Int)] = {
    State{m => (in, m) match {
        case (Coin, m @ Machine(_,     candies, _)) if candies > 0 => extractState(Machine(false, m.candies,     m.coins + 1))
        case (Turn, m @ Machine(false, candies, _)) if candies > 0 => extractState(Machine(true,  m.candies - 1, m.coins))
        case (_, m) => extractState(m)
      }
    }    
  }
  
  def extractState(m: Machine): ((Int, Int), Machine) = ((m.coins, m.candies), m)
  
  // Exercise 6.10:
  def unit[S,A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = {
    //def go(fs: List[State[S,A]], acc: State[S, List[A]]): State[S, List[A]] = {
    //    if (fs == Nil)
    //      acc
    //    else
    //      go(fs.tail, acc.map2(fs.head)((list, a) => a :: list))
    //}    
    //val acc = go(fs, unit(List[A]()))
    //acc.map(list => list.reverse)
    
    fs.foldRight(unit[S,List[A]](List[A]()))((sa, acc) => sa.map2(acc)(_ :: _))
  }    

  // Exercise 6.10 (extra mile):
  val _int: Rand[Int] = State(_.nextInt)

  def _nonNegativeInt: Rand[Int] = {
    _int.map {i =>
      if (i > 0)
        i
      else if (i > Int.MinValue)
        -i
      else
        0
    }
  } 

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(_int))
  }
  
}
