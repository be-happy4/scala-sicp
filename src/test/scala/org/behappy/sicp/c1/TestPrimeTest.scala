package org.behappy.sicp.c1

import org.behappy.sicp.c1.PrimeTest.*
import org.behappy.sicp.lang.*
import org.scalatest.funsuite.AnyFunSuite

class TestPrimeTest extends AnyFunSuite:
  test("test prime"):
    assert(prime(2))
    assert(!prime(4))
    assert(prime(97))
    assert(prime(197))
    assert(!prime(1127))
    assert(prime(274876858367L))

  test("test fermat-test"):
    assert(fast_prime(2))
    assert(!fast_prime(4))
    assert(fast_prime(97))
    assert(fast_prime(197))
    assert(!fast_prime(1127))
    assert(fast_prime(274876858367L))

  test("test carmichael for fermat-test"):
    assert(fast_prime(561))
    assert(fast_prime(1105))
    assert(fast_prime(1729))
    assert(fast_prime(2465))
    assert(fast_prime(2821))
    assert(fast_prime(6601))

  test("test Miller-Rabin-test"):
    assert(miller_rabin_test(2))
    assert(!miller_rabin_test(4))
    assert(miller_rabin_test(97))
    assert(miller_rabin_test(197))
    assert(!miller_rabin_test(1127))
    assert(miller_rabin_test(274876858367L))

  test("test carmichael for Miller-Rabin-test"):
    assert(!miller_rabin_test(561))
    assert(!miller_rabin_test(1105))
    assert(!miller_rabin_test(1729))
    assert(!miller_rabin_test(2465))
    assert(!miller_rabin_test(2821))
    assert(!miller_rabin_test(6601))

