package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FNum.*

import scala.annotation.tailrec


object Sum:
  def filtered_accumulate(combiner: (FNum, FNum) => FNum,
                          null_value: FNum, predicate: FNum => Boolean,
                          term: FNum => FNum, a: FNum,
                          next: FNum => FNum, b: FNum): FNum =
    @tailrec
    def iter(a: FNum, result: FNum): FNum =
      if (a > b) result
      else iter(next(a),
        if (predicate(a)) combiner(result, term(a))
        else result)

    iter(a, null_value)

  def accumulate(combiner: (FNum, FNum) => FNum, null_value: FNum,
                 term: FNum => FNum, a: FNum, next: FNum => FNum, b: FNum): FNum =
    filtered_accumulate(combiner, null_value, _ => true, term, a, next, b)

  def sum(term: FNum => FNum, a: FNum, next: FNum => FNum, b: FNum): FNum =
    accumulate(_ + _, 0, term, a, next, b)

  def product(term: FNum => FNum, a: FNum, next: FNum => FNum, b: FNum): FNum =
    accumulate(_ * _, 1, term, a, next, b)

  def sum_integers(a: FNum, b: FNum): FNum =
    sum(identify, a, inc, b)

  def sum_cubes(a: FNum, b: FNum): FNum =
    sum(cube, a, inc, b)

  def pi_sum(a: FNum, b: FNum): FNum =
    sum(x => 1.0 / x * x * 2, a, x => x + 4, b)

  def integral(f: FNum => FNum, a: FNum, b: FNum, dx: FNum): FNum =
    sum(f, a + dx / 2.0, x => x + dx, b) * dx

  def simpson(f: FNum => FNum, a: FNum, b: FNum, n: FNum): FNum =

    def h = (b - a) / n

    def y(k: FNum) = f(a + k * h)

    def factor(k: FNum): FNum =
      if ((k equals 0) || (k equals n)) 1
      else if (k.isOdd) 4
      else 2

    def term: FNum => FNum = k => (factor(k) * y(k))

    if (n.isOdd) error("n can't be odd")
    else h / 3 * sum(term, exact2inexact(0), inc, n)

  def main(args: Array[String]): Unit =
    println(8 * pi_sum(1, 1000))
    println(integral(cube, 0, 1, 0.01))
    println(integral(cube, 0, 1, 0.001))
    println(simpson(cube, 0, 1, 100))
    println(simpson(cube, 0, 1, 1000))

