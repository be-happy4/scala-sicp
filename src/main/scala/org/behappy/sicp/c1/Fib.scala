package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec

object Fib:
  def fib_tree(n: Num): Num =
    if (`=`(n, 0)) 0
    else if (`=`(n, 1)) 1
    else `+`(fib_tree(`-`(n, 1)), fib_tree(`-`(n, 2)))
    
  def fib_matrix(n: Num): Num =
    @tailrec
    def fib_iter(a: Num, b: Num, p: Num, q: Num, count: Num): Num =
      if (`=`(count, 0)) b
      else if (even(count))
        fib_iter(a, b,
          `+`(square(p), square(q)),
          `+`(*(2, p, q), square(q)),
          /(count, 2))
      else fib_iter(
        `+`(*(b, q), *(a, q), *(a, p)),
        `+`(*(b, p), *(a, q)),
        p, q, `-`(count, 1)
      )

    fib_iter(1, 0, 0, 1, n)