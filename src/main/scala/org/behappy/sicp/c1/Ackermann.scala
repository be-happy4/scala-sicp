package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FInt.*


object Ackermann:
  def A(x: FInt, y: FInt): FInt =
    if (y equals 0) 0
    else if (x equals 0) y * 2
    else if (y equals 1) 2
    else A(
      x.dec,
      A(x, y.dec))

  def f(n: FInt): FInt = A(0, n) // 2n
  def g(n: FInt): FInt = A(1, n) // 2^n
  def h(n: FInt): FInt = A(2, n) // 2^2...^2 -- n
