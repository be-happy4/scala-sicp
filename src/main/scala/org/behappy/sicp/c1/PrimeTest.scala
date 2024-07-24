package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FNum.*

import scala.annotation.tailrec

/**
 * @see [[https://en.wikipedia.org/wiki/Fermat_primality_test]]
 * @see [[https://en.wikipedia.org/wiki/Carmichael_number]]
 */
object PrimeTest:
  def prime(n: FNum): Boolean =
    @tailrec
    def find_divisor(n: FNum, test_divisor: FNum): FNum =
      if (test_divisor.square > n) n
      else if (test_divisor divides n) test_divisor
      else find_divisor(n, test_divisor.inc)

    def smallest_divisor(n: FNum) =
      find_divisor(n, 2)

    n equals smallest_divisor(n)

  @tailrec
  def fast_prime(n: FNum, times: FNum = 20): Boolean =
    def expmod(base: FNum, exp: FNum, m: FNum): FNum =
      if (exp equals 0) 1
      else if (isEven(exp)) expmod(base, exp / 2, m).square % m
      else base * expmod(base, exp.dec, m) % m

    def fermat_test(n: FNum): Boolean =
      def try_it(a: FNum) = expmod(a, n, n) equals a

      try_it(random(1, n))

    if (times equals 0) true
    else if (fermat_test(n)) fast_prime(n, times.dec)
    else false

  def miller_rabin_test(n: FNum, times: FNum = 0): Boolean =
    def expmod(base: FNum, exp: FNum, m: FNum): FNum =
      if (exp equals 0) 1
      else if (nontrivial_square_root(base, m)) 0
      else if (isEven(exp)) expmod(base, exp / 2, m).square % m
      else base * expmod(base, exp.dec, m) % m

    def nontrivial_square_root(a: FNum, n: FNum): Boolean =
      (a ne 1) && (a ne n.dec) && (1 equals a.square % n)

    @tailrec
    def test_iter(n: FNum, times: FNum): Boolean =
      if (times equals 0) true
      else if (expmod(random(1, n), n.dec, n) equals 1)
        test_iter(n, dec(times))
      else false

    if (times equals 0) test_iter(n, ceiling(n / 2))
    else test_iter(n, times)
