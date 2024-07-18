package org.behappy.sicp.c1

import org.behappy.sicp.c1.Newton.dx
import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.Num.*

import scala.annotation.tailrec

object FixedPoint:
  @tailrec
  def search(f: NumOp1, neg_point: Num, pos_point: Num): Num =
    def close_enough(x: Num, y: Num, delta: Num = 0.001): Boolean =
      <(abs(`-`(x, y)), delta)

    val midpoint = average(neg_point, pos_point)
    if (close_enough(neg_point, pos_point)) midpoint
    else
      val test_value = f(midpoint)
      if (positive(test_value)) search(f, neg_point, midpoint)
      else if (negative(test_value)) search(f, midpoint, pos_point)
      else midpoint

  def half_interval_method(f: NumOp1, a: Num, b: Num): Num =
    val a_value = f(a)
    val b_value = f(b)
    if (and(negative(a_value), positive(b_value))) search(f, a, b)
    else if (and(negative(b_value), positive(a_value))) search(f, b, a)
    else error("Value are not of opposite sign")

  def tolerance = 0.00001

  def fixed_point(f: NumOp1, first_guess: Num): Num =
    def close_enough(v1: Num, v2: Num): Boolean =
      <(abs(`-`(v1, v2)), tolerance)

    def improve(guess: Num): Num = f(guess)

    iterative_improve(close_enough, improve)(first_guess)

  def sqrt(x: Num): Num =
    fixed_point(y => average(y, /(x, y)), 1.0)

  def cont_frac(n: NumOp1, d: NumOp1, k: Num): Num =
    @tailrec
    def iter(i: Num = k, result: Num = 0): Num =
      if (`=`(i, 0)) result
      else iter(dec(i), /(n(i), `+`(result, d(i))))

    iter()

  def average_damp(f: NumOp1): NumOp1 =
    x => average(x, f(x))

  def cube_root(x: Num): Num =
    fixed_point(average_damp(y => `/`(x, y)), 1.0)

  def fixed_point_of_transform
  (g: NumOp1, transform: NumOp1 => NumOp1, guess: Num): Num =
    fixed_point(transform(g), guess)

  def smooth(f: NumOp1): NumOp1 =
    x => average(f(`-`(x, dx)), f(x), f(`+`(x, dx)))

  def average_damp_n_times(f: NumOp1, n: Num): NumOp1 =
    repeated(average_damp, n)(f)

  def damped_nth_root(n: Num, damp_times: Num): NumOp1 =
    x => fixed_point(
      average_damp_n_times(
        y => /(x, expt(y, dec(n))),
        damp_times),
      1.0)

  def nth_root(n: Num): NumOp1 =
    damped_nth_root(n, log2(n))

  def iterative_improve(close_enough: NumPre2, improve: NumOp1): NumOp1 =
    @tailrec
    def `try`(guess: Num): Num =
      val next = improve(guess)
      if (close_enough(guess, next)) next
      else `try`(next)
    `try`

  def main(args: Array[String]): Unit =
    println(half_interval_method(sin, 2.0, 4.0))
    println(half_interval_method(x => `-`(cube(x), *(2, x), 3), 1.0, 2.0))
    println(fixed_point(cos, 1.0))
    println(fixed_point(y => `+`(sin(y), cos(y)), 1.0))
    println(/(2, `+`(1, sqrt(5))))
    for i <- List(10, 100, 1000, 10000) do
      println(f"$i => ${cont_frac(_ => 1, _ => 1, 10)}")

    println(nth_root(4)(*(3, 3, 3, 3)))
    println(sqrt(9))