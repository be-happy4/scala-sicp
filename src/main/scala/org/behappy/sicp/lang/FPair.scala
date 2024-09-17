package org.behappy.sicp.lang


trait FPair[+A, +B]:
  def car: A
  def cdr: B
  def head: A = car
  def tail: B = cdr


private class PairImpl[+A, +B](
  override val car: A,
  override val cdr: B,
) extends FPair[A, B]

object FPair:
  def apply[A, B](a: A, b: B): FPair[A, B] =
    PairImpl(a, b)
///**
// * Technology type
// */
//private trait Pair0[A, B]:
//  def apply[R](f: Fun2[A, B, R]): R