package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FNum.*

import scala.annotation.tailrec

object Fib:
  def fib_tree(n: FNum): FNum =
    if (n equals 0) 0
    else if (n equals 1) 1
    else fib_tree(n - 1) + fib_tree(n - 2)

  def fib_matrix(n: FNum): FNum =
    @tailrec
    def fib_iter(a: FNum, b: FNum, p: FNum, q: FNum, count: FNum): FNum =
      if (count equals 0) b
      else if (isEven(count))
        fib_iter(a, b,
          square(p) + square(q),
          2 * p * q + square(q),
          count / 2)
      else fib_iter(
        b * q + a * q + a * p,
        b * p + a * q,
        p, q, count - 1
      )

    fib_iter(1, 0, 0, 1, n)