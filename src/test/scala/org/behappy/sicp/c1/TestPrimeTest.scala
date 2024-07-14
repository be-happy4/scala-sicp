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

  test("test fermat test"):
    assert(fast_prime(2))
    assert(!fast_prime(4))
    assert(fast_prime(97))
    assert(fast_prime(197))
    assert(!fast_prime(1127))
    assert(fast_prime(274876858367L))
