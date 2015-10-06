package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Exercise 4.1:
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] =
    map (x => f(x)) getOrElse(None)
    
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map (x => Some(x)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    map (x => if (f(x)) Some(x) else None) getOrElse(None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
    
  // Exercise 4.2:
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).map{m =>
      val sumOfSquares = xs.foldLeft(0.0)((sum, x) => sum + math.pow(x - m, 2))
      sumOfSquares / xs.length
    }
  }

  // Exercise 4.3:
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    //for (x <- a; y <- b) yield f(x, y)
    a.flatMap{x => b.map(y => f(x, y))}    
  }

  // Exercise 4.4:
  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    // Fold does not short circuit!
    //l.foldRight(Some(Nil): Option[List[A]]){(optionA, optionList) => for (list <- optionList; a <- optionA) yield a :: list}
    l.foldRight(Some(Nil): Option[List[A]]){(optionA, optionList) => optionList.flatMap(list => optionA.map(a => a :: list))}
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}