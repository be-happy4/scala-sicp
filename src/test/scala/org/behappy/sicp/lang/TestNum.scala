package org.behappy.sicp.lang

import org.behappy.sicp.lang.FNum.{FNum, expt, factorial, gcd, int2FNum, square}
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite


class TestNum extends munit.FunSuite:
  test("test ="):
    assert(1 == 1)
    assert(1 == 1)
    assert(0 ne 1)

  test("test +"):
    assert(1 == 1)
    assert(3 == 1 + 2)
    assert(15 == 1 + 2 + 3 + 4 + 5)

  test("test *"):
    assert(2 == 1 * 2)
    assert(120 == 1 * 2 * 3 * 4 * 5)

  test("test square"):
    assert(9 == 3.square)
    assert(16 == square(4))
    assert(121 == square(11))

  test("test expt"):
    assert(1 == 2.expt(0))
    assert(128 == 2.expt(7))
    assert(1024 == 2.expt(10))

  test("test factorial"):
    assert(120 == factorial(5))
    assert(720 == factorial(6))

  test("test gcd"):
    assert(12 == (24 gcd 36))


