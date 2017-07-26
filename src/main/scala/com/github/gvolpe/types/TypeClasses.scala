package com.github.gvolpe.types

import scala.language.higherKinds

object TypeClasses {

  implicit object listMonad extends Monad[List] {
    override def bind[A, B](ma: List[A], k: (A) => List[B]) = ma.flatMap(k)
    override def pure[A](a: A) = List(a)
  }

  implicit class ListWithBind[A](xs: List[A]) {
    def >>=[B](f: A => List[B]): List[B] = listMonad.bind(xs, f)
  }

  implicit class WithPure[A](a: A) {
    def pure: List[A] = listMonad.pure(a)
  }

  val as: List[String] = List("a", "b", "c")
  val k: String => List[String] = x => List(x + "0" , x + "1")

  implicitly[Monad[List]].bind(as, k)

  as >>= k

  implicitly[Monad[List]].pure("a")

  "a".pure

  //--------------------------------------------

  sealed trait BTree[+A]
  case object Empty extends BTree[Nothing]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], a: A, right: BTree[A]) extends BTree[A]

  implicit object bTreeFoldable extends Foldable[BTree] {
    override def foldr[A, B](ta: BTree[A])(f: (A) => (B) => B)(seed: B) = ta match {
      case Empty    => seed
      case Leaf(a)  => f(a)(seed)
      case Fork(left, a, right) => f(a)(foldr(left)(f)(foldr(right)(f)(seed)))
    }
  }

  implicit class FoldableBTree[A](tree: BTree[A]) {
    def foldr[B](f: A => B => B)(seed: B): B = bTreeFoldable.foldr(tree)(f)(seed)
  }

  val tree = Fork(Leaf(1), 2, Leaf(3))
  tree.foldr((x: Int) => (y: Int) => x + y)(0)
  implicitly[Foldable[BTree]].foldr(tree)((x: Int) => (y: Int) => x + y)(0)

}

trait Monad[M[_]] {
  def bind[A, B](ma: M[A], k: A => M[B]): M[B]
  def pure[A](a: A): M[A]
}

trait Foldable[T[_]] {
  def foldr[A, B](ta: T[A])(f: A => B => B)(seed: B): B
}