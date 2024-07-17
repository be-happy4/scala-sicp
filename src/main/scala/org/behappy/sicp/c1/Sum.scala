package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec


object Sum:
  def filtered_accumulate(combiner: (Num, Num) => Num,
                          null_value: Num, predicate: Num => Boolean,
                          term: Num => Num, a: Num,
                          next: Num => Num, b: Num): Num =
    @tailrec
    def iter(a: Num, result: Num): Num =
      if (>(a, b)) result
      else iter(next(a),
        if (predicate(a)) combiner(result, term(a))
        else result)

    iter(a, null_value)

  def accumulate(combiner: (Num, Num) => Num, null_value: Num,
                 term: Num => Num, a: Num, next: Num => Num, b: Num): Num =
    filtered_accumulate(combiner, null_value, _ => true, term, a, next, b)

  def sum(term: Num => Num, a: Num, next: Num => Num, b: Num): Num =
    accumulate(`+`, 0, term, a, next, b)

  def product(term: Num => Num, a: Num, next: Num => Num, b: Num): Num =
    accumulate(`*`, 1, term, a, next, b)

  def sum_integers(a: Num, b: Num): Num =
    sum(identify, a, inc, b)

  def sum_cubes(a: Num, b: Num): Num =
    sum(cube, a, inc, b)

  def pi_sum(a: Num, b: Num): Num =
    sum(x => `/`(1.0, *(x, `+`(x, 2))), a, x => `+`(x, 4), b)

  def integral(f: Num => Num, a: Num, b: Num, dx: Num): Num =
    `*`(sum(f, `+`(a, /(dx, 2.0)), x => `+`(x, dx), b), dx)

  def simpson(f: Num => Num, a: Num, b: Num, n: Num): Num =

    def h = /(`-`(b, a), n)

    def y(k: Num) = f(`+`(a, *(k, h)))

    def factor(k: Num): Num =
      if (or(`=`(k, 0), `=`(k, n))) 1
      else if (odd(k)) 4
      else 2

    def term: Num => Num = k => `*`(factor(k), y(k))

    if (not(even(n))) error("n can't be odd")
    else *(/(h, 3), sum(term, exact2inexact(0), inc, n))

  def main(args: Array[String]): Unit =
    println(`*`(8, pi_sum(1, 1000)))
    println(integral(cube, 0, 1, 0.01))
    println(integral(cube, 0, 1, 0.001))
    println(simpson(cube, 0, 1, 100))
    println(simpson(cube, 0, 1, 1000))

