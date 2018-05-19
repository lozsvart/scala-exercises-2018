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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def contains[A](l: List[A], itemToFind: A): Boolean = foldLeft(l, false)((folded, item) => folded || item.equals(itemToFind))

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def tail[A](l: List[A]): List[A] = foldRight[A, (Option[A], List[A])](l, (Option.empty, Nil))((item, folded) => {
    val (buffer, result) = folded
    val nextResult = if (buffer.isEmpty) Nil else Cons(buffer.get, result)
    (Option.apply(item), nextResult)
  })._2

  def setHead[A](l: List[A], h: A): List[A] = foldRight[A, (List[A], List[A])](l, (Nil, Nil))((item, folded) => {
    val (lastItem, result) = folded
    val nextResult = lastItem match {
      case Nil => Cons(h, Nil)
      case Cons(wrappedItem, _) => Cons(h, Cons(wrappedItem, tail(result)))
    }
    (List(item), nextResult)
  })._2

  def drop[A](l: List[A], n: Int): List[A] = foldLeft[A, (Int, List[A])](l, (n, Nil))((folded, item) => {
    val (toSkip, result) = folded
    if (toSkip > 0)
      (toSkip - 1, result)
    else
      (toSkip, append(result, List(item)))
  })._2

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = foldLeft[A, (Boolean, List[A])](l, (true, Nil))((folded, item) => {
    val (continue, result) = folded
    if (continue)
      if (f(item))
        (true, append(result, List(item)))
      else
        (false, result)
    else
      folded
  })._2

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = foldLeft[A, (Boolean, List[A])](l, (true, Nil))((folded, item) => {
    val (dropping, result) = folded
    if (f(item) && dropping)
      folded
    else
      (false, append(result, List(item)))
  })._2

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((folded, item) => Cons(item, folded))

  def init[A](l: List[A]): List[A] = foldRight[A, (Boolean, List[A])](l, (true, Nil))((item, folded) => {
    val (shouldSkip, result) = folded
    if (shouldSkip)
      (false, result)
    else
      (false, Cons(item, result))
  })._2

  def reduce[A](l: List[A], z: A)(f: (A, A) ⇒ A): A = foldLeft(l, z)(f)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((folded, _) => folded + 1)

  def filter[A](l: List[A], f: A => Boolean): List[A] = foldRight[A, List[A]](l, Nil)((item, folded) => {
    if (f(item))
      Cons(item, folded)
    else
      folded
  })

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight[A, List[B]](l, Nil)((item, folded) => Cons(f(item), folded))

  def flatten[A](l: List[List[A]]): List[A] = foldLeft[List[A], List[A]](l, Nil)(append)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldLeft[A, List[B]](l, Nil)((folded, item) => append(folded, f(item)))

  def partition[A](l: List[A], p: A => Boolean): (List[A], List[A]) = foldRight[A, (List[A], List[A])](l, (Nil, Nil))((item, folded) => {
    val (truePart, falsePart) = folded
    if (p(item))
      (Cons(item, truePart), falsePart)
    else
      (truePart, Cons(item, falsePart))
  })

  def avg(list: List[Double]): Double = foldLeft[Double, (Double, Int)](list, (0.0, 0))((folded, item) => {
    val (avg, count) = folded
    ((avg * count + item) / (count + 1), count + 1)
  })._1

  def zipWith[A, B](list: List[A], other: List[B]): List[(A, B)] = foldLeft[A, (List[B], List[(A, B)])](list, (other, Nil))((folded, item) => {
    val (otherList, result) = folded
    otherList match {
      case Nil => folded
      case Cons(otherItem, _) => (tail(otherList), append(result, List((item, otherItem))))
    }
  })._2

  def hasSubsequence[A](list: List[A], subList: List[A]): Boolean = contains(foldLeft[A, List[List[A]]](list, List(subList))((folded, item) => {
    map(filter[List[A]](Cons(subList, folded), {
      case Nil => true
      case Cons(h, _) => h.equals(item)
    }))(tail)
  }), Nil)
}
