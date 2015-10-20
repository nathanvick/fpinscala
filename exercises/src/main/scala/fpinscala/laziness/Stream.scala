package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  // Exercise 5.1:
  def toList(): List[A] = foldRight(List[A]())(_ :: _)
  
  // Exercise 5.2/5.13:
  def take(n: Int): Stream[A] = {
    //this match {
    //  case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    //  case _ => Empty
    //}

    // Exercise 5.13:
    unfold(this, n) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case s @ Cons(h, t) => if (n > 0) t().drop(n - 1) else s
  }
  
  // Exercise 5.3/5.5/5.13:
  def takeWhile(p: A => Boolean): Stream[A] = {
    //foldRight(Empty:Stream[A])((a, acc) => if (p(a)) Stream.cons(a, acc) else Empty)

    // Exercise 5.13:
    unfold(this) { 
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  // Exercise 5.4:
  def forAll(p: A => Boolean): Boolean =
    //foldRight(true)((a, b) => p(a) && b)
    !exists(!p(_))

  // Exercise 5.6:
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // Exercise 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    //foldRight(Empty:Stream[B])((a, acc) => Stream.cons(f(a), acc))

    // Exercise 5.13:
    unfold(this) { 
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def filter(p: A => Boolean): Stream[A] =  
    foldRight(Empty:Stream[A])((a, acc) => if (p(a)) Stream.cons(a, acc) else acc)
  
  def append[B >: A](a: B): Stream[B] =
    foldRight(Stream(a))((a, acc) => Stream.cons(a, acc))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =  
    foldRight(Empty:Stream[B])((a, acc) => f(a).foldRight(acc)((a, acc) => Stream.cons(a, acc)))
  
  // Exercise 5.13:
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, s2) { 
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B, C](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this, s2) { 
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }
  }

  // Exercise 5.13b (FAIL):
  /*
  // REASON: zipWith does not handle Stream(1,2) startsWith Stream(1,2,3)!!!
  def startsWith1[B](s: Stream[B]): Boolean =
    s == Empty || this != Empty && zipWith(s)((a, b) => a == b).forAll(x => x)

  // REASON: zipAll returns an infinite stream
  def startsWith2[B](s: Stream[B]): Boolean = {
    zipAll(s).map{
      case (Some(a), Some(b)) => a == b
      case (None, Some(b)) => false
      case _ => true
    }.forAll(x => x)
  }
  */
  
  // Exercise 5.13b (SUCCESS):
  /*
  def existsSuffix(p: Stream[A] => Boolean) = {
    unfold(this) {
      case s @ Cons(h, t) => Some(p(s), t())
      case _ => None
    }.exists(x => x)
  }

  def hasSubsequence3[A](s: Stream[A]): Boolean =
    s == Empty || existsSuffix(_.startsWith(s))
  */

  // Exercise 5.13b/5.15
  def hasSubsequence[A](s: Stream[A]): Boolean =
    s == Empty || this.tails.exists(_ startsWith s)
  
  // Exercise 5.14:  
  def startsWith[B](s: Stream[B]): Boolean = {
    unfold(this, s) { 
      case (Cons(h1, t1), Cons(h2, t2)) => Some(h1() == h2(), (t1(), t2()))
      case (Empty, Cons(_, t2)) => Some(false, (Empty, t2()))
      case _ => None
    }.forAll(x => x)
  }  
    
  // Exercise 5.15:
  def tails[Stream[Stream[A]]] = {
    unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case _ => None
    }    
  }      

  // Exercise 5.16 (FAIL):
  /*
  // REASON: unfold works from left to right
  def scanLeft[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    unfold(this, z) {
      case (Cons(h, t), s) =>
        val wip = f(h(), s)
        Some(wip, (t(), wip))
      case _ => None
    }
  }
  */
  
  // Exercise 5.16:  
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this match {
    case Cons(h,t) =>
      if (t() == Empty) {
        Stream(f(h(), z), z)
      } else {
        t().scanRight(z)(f) match {
          case wipS @ Cons(wipH, _) => cons(f(h(), wipH()), wipS)        
        }
      }
    case _ => Empty
  }
  
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  // Exercise 5.8/5.12:
  def constant[A](a: A): Stream[A] = 
    //Stream.cons(a, constant(a))
    unfold(a)(s => Some(s, s))

  // Exercise 5.9/5.12:
  def from(n: Int): Stream[Int] =
    //Stream.cons(n, from(n + 1))
    unfold(n)(s => Some(s, s + 1))

  // Exercise 5.10/5.12:
  def fibs: Stream[Int] = {
    //def go(a: Int, b: Int): Stream[Int] = Stream.cons(a, go(b, a + b))
    //go(0, 1)

    unfold(0, 1){case (a, b) => Some(a, (b, a + b))}
  }
  
  // Exercise 5.11:
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map{case (a, s) => Stream.cons(a, unfold(s)(f))}.getOrElse(Empty)
  }
}