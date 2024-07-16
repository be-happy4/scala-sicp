package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec

object NewtonIterate extends App:
  def sqrt(x: Num, delta: Num = 0.001): Num =
    def good_enough(guess: Num, x: Num): Boolean =
      <(abs(`-`(square(guess), x)), delta)

    def improve(guess: Num): Num =
      average(guess, /(x, guess))

    @tailrec
    def sqrt_iter(guess: Num): Num =
      if (good_enough(guess, x)) guess
      else sqrt_iter(improve(guess))

    sqrt_iter(1)

