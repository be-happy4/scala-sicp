package org.behappy.sicp.c1

import org.behappy.sicp.lang.*

import scala.annotation.tailrec

/**
 * @see [[https://en.wikipedia.org/wiki/Fermat_primality_test]]
 * @see [[https://en.wikipedia.org/wiki/Carmichael_number]]
 */
object PrimeTest:
  def prime(n: Num): Boolean =
    @tailrec
    def find_divisor(n: Num, test_divisor: Num): Num =
      if (>(square(test_divisor), n)) n
      else if (divides(test_divisor, n)) test_divisor
      else find_divisor(n, `+`(test_divisor, 1))

    def smallest_divisor(n: Num) =
      find_divisor(n, 2)

    `=`(n, smallest_divisor(n))

  @tailrec
  def fast_prime(n: Num, times: Num = 20): Boolean =
    def expmod(base: Num, exp: Num, m: Num): Num =
      if (`=`(exp, 0)) 1
      else if (even(exp)) %(square(expmod(base, /(exp, 2), m)), m)
      else %(*(base, expmod(base, `-`(exp, 1), m)), m)

    def fermat_test(n: Num): Boolean =
      def try_it(a: Num) = `=`(expmod(a, n, n), a)

      try_it(`+`(1, random(`-`(n, 1))))

    if (`=`(times, 0)) true
    else if (fermat_test(n)) fast_prime(n, `-`(times, 1))
    else false

  def miller_rabin_test(n: Num, times: Num = 0): Boolean =
    def expmod(base: Num, exp: Num, m: Num): Num =
      if (`=`(exp, 0)) 1
      else if (nontrivial_square_root(base, m)) 0
      else if (even(exp)) %(square(expmod(base, /(exp, 2), m)), m)
      else %(*(base, expmod(base, `-`(exp, 1), m)), m)

    def nontrivial_square_root(a: Num, n: Num): Boolean =
      and(not(`=`(a, 1)),
        not(`=`(a, `-`(n, 1))),
        `=`(1, %(square(a), n)))

    @tailrec
    def test_iter(n: Num, times: Num): Boolean =
      if (`=`(times, 0)) true
      else if (`=`(expmod(random(1, n), dec(n), n), 1))
        test_iter(n, dec(times))
      else false

    if (`=`(times, 0)) test_iter(n, ceiling(/(n, 2)))
    else test_iter(n, times)
