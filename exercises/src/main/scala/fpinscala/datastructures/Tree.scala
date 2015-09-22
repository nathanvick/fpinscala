package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25:
  def treeSize[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + treeSize(left) + treeSize(right)
  }
  
  // Exercise 3.26:
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => math.max(maximum(left), maximum(right))
  }

  // Exercise 3.27:
  def depth[A](t: Tree[A]): Int = {
    def go[A](t: Tree[A], acc: Int): Int = t match {
      case Leaf(value) => acc
      case Branch(left, right) => math.max(go(left, acc + 1), go(right, acc + 1))
    }
    go(t, 0)
  }

  // Exercise 3.28:
  def map[A, B](t: Tree[A])(f: A => B ): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29:
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }  

  def treeSize329[A](t: Tree[A]): Int = 
    fold(t){a => 1}{(b1, b2) => b1 + b2 + 1} 
  
  def maximum329(t: Tree[Int]): Int = 
    fold(t){a => a}{(b1, b2) => math.max(b1, b2)}

  def depth329[A](t: Tree[A]): Int = 
    fold(t){a => 1}{(b1, b2) => math.max(b1 + 1, b2 + 1)} - 1

  def map329[A, B](t: Tree[A])(f: A => B ): Tree[B] = 
    fold(t){a => Leaf(f(a)): Tree[B]}{(b1, b2) => Branch(b1, b2)} 
  
}