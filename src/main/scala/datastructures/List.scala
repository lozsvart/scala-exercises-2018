package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???

  def sum(ints: List[Int]): Int = ???

  def product(ds: List[Double]): Double = ???

  def append[A](a1: List[A], a2: List[A]): List[A] = ???

  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def reverse[A](l: List[A]): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def reduce[A](l: List[A], z: A)(f: (A, A) ⇒ A): A = ???

  def length[A](l: List[A]): Int = ???

  def filter[A](l: List[A], f: A => Boolean): List[A] = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def flatten[A](l: List[List[A]]): List[A] = ???

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = ???

  def partition[A](l: List[A], p: A => Boolean): (List[A], List[A]) = ???

  def avg(list: List[Double]): Double = ???

  def zipWith[A, B](list: List[A], other: List[B]): List[(A, B)] = ???

  def hasSubsequence[A](list: List[A], subList: List[A]): Boolean = ???
}
