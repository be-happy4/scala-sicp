package org.behappy.sicp.c1

import org.behappy.sicp.c1.FixedPoint.fixed_point
import org.behappy.sicp.lang.*

object Newton:
  def sqrt(x: Num): Num =
    newtons_method(y => `-`(square(y), x), 1.0)

  def deriv(g: NumFun): NumFun =
    x => /(`-`(g(`+`(x, dx)), g(x)), dx)

  def dx = 0.00001

  def newton_transform(g: NumFun): NumFun =
    x => `-`(x, /(g(x), deriv(g)(x)))

  def newtons_method(g: NumFun, guess: Num): Num =
    fixed_point(newton_transform(g), guess)

  def main(args: Array[String]): Unit =
    println(sqrt(9.0))
    println(sqrt(2))
    println(deriv(cube)(5))

