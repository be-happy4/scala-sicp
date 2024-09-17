package org.behappy.sicp.c1

import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FInt.FInt
import org.behappy.sicp.lang.FInt.square
import org.behappy.sicp.lang.FInt.random

import scala.annotation.tailrec

/**
 * @see [[https://en.wikipedia.org/wiki/Fermat_primality_test]]
 * @see [[https://en.wikipedia.org/wiki/Carmichael_number]]
 */
object PrimeTest:
  def prime(n: FInt): Boolean =
    @tailrec
    def find_divisor(n: FInt, test_divisor: FInt): FInt =
      if (test_divisor.square > n) n
      else if (n % test_divisor == 0) test_divisor
      else find_divisor(n, test_divisor + 1)

    def smallest_divisor(n: FInt) =
      find_divisor(n, 2)

    n == smallest_divisor(n)

  @tailrec
  def fast_prime(n: FInt, times: FInt = 20): Boolean =
    def expmod(base: FInt, exp: FInt, m: FInt): FInt =
      if (exp == 0) 1
      else if (exp % 2 == 0) expmod(base, exp / 2, m).square % m
      else base * expmod(base, exp - 1, m) % m

    def fermat_test(n: FInt): Boolean =
      def try_it(a: FInt) = expmod(a, n, n) equals a

      try_it(random(1, n))

    if (times equals 0) true
    else if (fermat_test(n)) fast_prime(n, times - 1)
    else false

  /**
   * Probabilistic methods
   */
  def miller_rabin_test(n: FInt, times: FInt = 0): Boolean =
    def expmod(base: FInt, exp: FInt, m: FInt): FInt =
      if (exp == 0) 1
      else if (nontrivial_square_root(base, m)) 0
      else if (exp % 2 == 0) expmod(base, exp / 2, m).square % m
      else base * expmod(base, exp - 1, m) % m

    def nontrivial_square_root(a: FInt, n: FInt): Boolean =
      (a != 1) && (a != n - 1) && (1 == a.square % n)

    @tailrec
    def test_iter(n: FInt, times: FInt): Boolean =
      if (times equals 0) true
      else if (expmod(random(1, n), n - 1, n) equals 1)
        test_iter(n, times - 1)
      else false

    if (times equals 0) test_iter(n, (n + 1) / 2)
    else test_iter(n, times)
