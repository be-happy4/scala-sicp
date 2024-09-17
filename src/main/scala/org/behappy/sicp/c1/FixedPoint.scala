package org.behappy.sicp.c1

import org.behappy.sicp.c1.Newton.dx
import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FInt.FInt
import org.behappy.sicp.lang.FNum.*

import scala.annotation.tailrec

object FixedPoint:
  @tailrec
  def search(f: NumOp1, neg_point: FNum, pos_point: FNum): FNum =
    def close_enough(x: FNum, y: FNum, delta: FNum = 0.001): Boolean =
      (x - y).abs < delta

    val midpoint = neg_point.average(pos_point)
    if (close_enough(neg_point, pos_point)) midpoint
    else
      val test_value = f(midpoint)
      if (test_value.isPositive) search(f, neg_point, midpoint)
      else if (test_value.isNegative) search(f, midpoint, pos_point)
      else midpoint

  def half_interval_method(f: NumOp1, a: FNum, b: FNum): FNum =
    val a_value = f(a)
    val b_value = f(b)
    if (a_value.isNegative && b_value.isPositive) search(f, a, b)
    else if (b_value.isNegative && a_value.isPositive) search(f, b, a)
    else error("Value are not of opposite sign")

  private def tolerance = 0.00001

  def fixed_point(f: NumOp1, first_guess: FNum): FNum =
    def close_enough(v1: FNum, v2: FNum): Boolean = (v1 - v2).abs < tolerance

    def improve(guess: FNum): FNum = f(guess)

    iterative_improve(close_enough, improve)(first_guess)

  def sqrt(x: FNum): FNum =
    fixed_point(y => y.average(x / y), 1.0)

  def cont_frac(n: NumOp1, d: NumOp1, k: FNum): FNum =
    @tailrec
    def iter(i: FNum = k, result: FNum = 0): FNum =
      if (i equals 0) result
      else iter(i - 1, n(i) / result + d(i))

    iter()

  def average_damp(f: NumOp1): NumOp1 =
    x => x.average(f(x))

  def cube_root(x: FNum): FNum =
    fixed_point(average_damp(y => x / y), 1.0)

  def fixed_point_of_transform
  (g: NumOp1, transform: NumOp1 => NumOp1, guess: FNum): FNum =
    fixed_point(transform(g), guess)

  def smooth(f: NumOp1): NumOp1 =
    x => f(x - dx).average(f(x), f(x + dx))

  def average_damp_n_times(f: NumOp1, n: FInt): NumOp1 =
    repeated(average_damp, n)(f)

  def damped_nth_root(n: FNum, damp_times: FInt): NumOp1 =
    x => fixed_point(
      average_damp_n_times(
        y => x / y.pow((n - 1).toInt),
        damp_times),
      1.0)

  def nth_root(n: FNum): NumOp1 =
    damped_nth_root(n, n.log2.toFInt)

  def iterative_improve(close_enough: NumPre2, improve: NumOp1): NumOp1 =
    @tailrec
    def `try`(guess: FNum): FNum =
      val next = improve(guess)
      if (close_enough(guess, next)) next
      else `try`(next)

    `try`

  def main(args: Array[String]): Unit =
    println(half_interval_method(sin, 2.0, 4.0))
    println(half_interval_method(x => cube(x) - 2 * x - 3, 1.0, 2.0))
    println(fixed_point(cos, 1.0))
    println(fixed_point(y => y.sin + y.cos, 1.0))
    println(2 / (1 + sqrt(5)))
    for i <- List(10, 100, 1000, 10000) do
      println(f"$i => ${cont_frac(_ => 1, _ => 1, 10)}")

    println(nth_root(4)(3 * 3 * 3 * 3))
    println(sqrt(9))