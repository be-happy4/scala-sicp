package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec

/**
 * @see [[https://en.wikipedia.org/wiki/Fermat_primality_test]]
 * @see [[https://en.wikipedia.org/wiki/Carmichael_number]]
 */
object PrimeTest extends App:
  @tailrec
  def find_divisor(n: Num, test_divisor: Num): Num =
    if (>(square(test_divisor), n)) n
    else if (divides(test_divisor, n)) test_divisor
    else find_divisor(n, `+`(test_divisor, 1))

  def smallest_divisor(n: Num) =
    find_divisor(n, 2)
  
  def prime(n: Num): Boolean =
    `=`(n, smallest_divisor(n))

  def expmod(base: Num, exp: Num, m: Num): Num =
    if (`=`(exp, 0)) 1
    else if (even(exp)) %(square(expmod(base, /(exp, 2), m)), m)
    else %(*(base, expmod(base, `-`(exp, 1), m)), m)

  def fermat_test(n: Num): Boolean =
    def try_it(a: Num) = `=`(expmod(a, n, n), a)

    try_it(`+`(1, random(`-`(n, 1))))

  @tailrec
  def fast_prime(n: Num, times: Num = 20): Boolean =
    if (`=`(times, 0)) true
    else if (fermat_test(n)) fast_prime(n, `-`(times, 1))
    else false
