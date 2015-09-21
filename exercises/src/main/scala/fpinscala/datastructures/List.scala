package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2:
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, t) => t
  }

  // Exercise 3.3:
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4:
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case neg if neg < 0 => throw new IllegalArgumentException("drop a negative number of elements")
    case 0 => l
    case pos => l match {
      case Nil => throw new UnsupportedOperationException("drop a positive number of elements from an empty list") 
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Exercise 3.5:
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =  l match {
    case Nil => Nil
    case Cons(a, t) => if (f(a)) dropWhile(t, f) else l 
  }

  // Exercise 3.6:
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("init of Nil")
      case Cons(a, t:Cons[A]) => go(t, Cons(a, acc))
      case Cons(a, t) => acc
    }

    def reverse[A](l: List[A]): List[A] = {
      @annotation.tailrec
      def go[A](l: List[A], acc: List[A]): List[A] = l match {
        case Nil => acc
        case Cons(a, t) => go(t, Cons(a, acc))
      }
      go(l, Nil)
    }

    reverse(go(l, Nil))
  }

  // Exercise 3.7: how to short-circuit foldRight
  // (1) throw exception
  // (2) redefine foldRight return type to indicate whether to continue

  // Exercise 3.9:
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((a, z) => z + 1)

  // Exercise 3.10:
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, t) => foldLeft(t, f(z, a))(f)
  }

  // Exercise 3.11:
  def sum310(ns: List[Int]) =
    foldLeft(ns, 0.0)(_ + _)

  def product310(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length310[A](l: List[A]): Int =
    foldLeft(l, 0)((z, a) => z + 1)

  // Exercise 3.12:
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((z, a) => Cons(a, z))

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
