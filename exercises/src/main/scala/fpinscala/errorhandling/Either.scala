package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  // Exercise 4.6:
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(y) => Right(f(y))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(y) => f(y)
    case Left(x) => Left(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(y) => this
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    //this.flatMap(aa => b.map(bb => f(aa, bb)))
    for (aa <- this; bb <- b) yield f(aa, bb)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // Exercise 4.7:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    // foldRight encounters errors at the end of the list before encountering errors at the beginning of the list:
    //es.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => for (bs <- acc; b <- f(a)) yield b :: bs)
    es.foldLeft(Right(Nil): Either[E, List[B]])((acc, a) => for (bs <- acc; b <- f(a)) yield b :: bs) map (acc => acc.reverse)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    // foldRight encounters errors at the end of the list before encountering errors at the beginning of the list:
    //es.foldRight(Right(Nil): Either[E, List[A]])((e, acc) => for (as <- acc; a <- e) yield a :: as)
    es.foldLeft(Right(Nil): Either[E, List[A]])((acc, e) => for (as <- acc; a <- e) yield a :: as) map (acc => acc.reverse)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}