/*
sbt
project exercises
console
*/

import fpinscala.datastructures._
import fpinscala.datastructures.List._

// Exercise 3.8:
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))