package org.behappy.sicp.lang

/**
 * Technology type
 */
private trait Pair0[A, B]:
  def apply[R](f: Fun2[A, B, R]): R

opaque type Pair[A, B] = Pair0[A, B]

def cons[A, B](a: A, b: B): Pair[A, B] = new Pair[A, B]:
  override def apply[R](f: Fun2[A, B, R]): R = f(a, b)

def car[A, B](x: Pair[A, B]): A =
  x((a, _) => a)

def cdr[A, B](x: Pair[A, B]): B =
  x((_, b) => b)

