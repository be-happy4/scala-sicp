package org.behappy.sicp.lang

/**
 * Technology type
 */
private trait Pair0[A, B]:
  def apply[R](f: Fun2[A, B, R]): R

opaque type Pair[A, B] = Pair0[A, B]

object Pair:
  def apply[A, B](a: A, b: B): Pair[A, B] = new Pair[A, B]:
    override def apply[R](f: Fun2[A, B, R]): R = f(a, b)

  extension[A, B] (x: Pair[A, B])
    def car: A =
      x((a, _) => a)

    def cdr: B =
      x((_, b) => b)