package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.int2num


object Ackermann:
  def A(x: Num, y: Num): Num =
    if (`=`(y, 0)) 0
    else if (`=`(x, 0)) *(2, y)
    else if (`=`(y, 1)) 2
    else A(
      `-`(x, 1),
      A(x, `-`(y, 1)))

  def f(n: Num): Num = A(0, n) // 2n
  def g(n: Num): Num = A(1, n) // 2^n
  def h(n: Num): Num = A(2, n) // 2^2...^2 -- n
