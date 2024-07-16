package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec

object FixedPoint extends App:
  @tailrec
  def search(f: Num => Num, neg_point: Num, pos_point: Num): Num =
    def close_enough(x: Num, y: Num, delta: Num = 0.001): Boolean =
      <(abs(`-`(x, y)), delta)

    val midpoint = average(neg_point, pos_point)
    if (close_enough(neg_point, pos_point)) midpoint
    else
      val test_value = f(midpoint)
      if (positive(test_value)) search(f, neg_point, midpoint)
      else if (negative(test_value)) search(f, midpoint, pos_point)
      else midpoint

  def half_interval_method(f: Num => Num, a: Num, b: Num): Num =
    val a_value = f(a)
    val b_value = f(b)
    if (and(negative(a_value), positive(b_value))) search(f, a, b)
    else if (and(negative(b_value), positive(a_value))) search(f, b, a)
    else error("Value are not of opposite sign")

  def tolerance = 0.00001

  def fixed_point(f: Num => Num, first_guess: Num): Num =
    def close_enough(v1: Num, v2: Num): Boolean =
      <(abs(`-`(v1, v2)), tolerance)

    @tailrec
    def `try`(guess: Num): Num =
      val next = f(guess)
      if (close_enough(guess, next)) next
      else `try`(next)

    `try`(first_guess)

  def sqrt(x: Num): Num =
    fixed_point(y => average(y, /(x, y)), 1.0)

  def cont_frac(n: Num => Num, d: Num => Num, k: Num): Num =
    @tailrec
    def iter(i: Num = k, result: Num = 0): Num =
      if (`=`(i, 0)) result
      else iter(dec(i), /(n(i), `+`(result, d(i))))

    iter()

  println(half_interval_method(sin, 2.0, 4.0))
  println(half_interval_method(x => `-`(cube(x), *(2, x), 3), 1.0, 2.0))
  println(fixed_point(cos, 1.0))
  println(fixed_point(y => `+`(sin(y), cos(y)), 1.0))
  println(/(2, `+`(1, sqrt(5))))
  for i <- List(10, 100, 1000, 10000) do
    println(f"$i => ${cont_frac(_ => 1, _ => 1, 10)}")