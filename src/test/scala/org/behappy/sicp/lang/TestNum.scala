package org.behappy.sicp.lang

import org.behappy.sicp.lang.FNum.*
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite

class TestNum extends AnyFunSuite:
  test("test ="):
    assert(1 equals 1)
    assert(1 equals 1)
    assert(0 ne 1)

  test("test +"):
    assert(1 equals 1.+())
    assert(3 equals 1 + 2)
    assert(15 equals 1 + 2 + 3 + 4 + 5)

  test("test *"):
    assert(1 equals 1.*())
    assert(2 equals 1 * 2)
    assert(120 equals 1 * 2 * 3 * 4 * 5)

  test("test square"):
    assert(9 equals square(3))
    assert(16 equals square(4))
    assert(121 equals square(11))

  test("test expt"):
    assert(1 equals 2.expt(0))
    assert(128 equals 2.expt(7))
    assert(1024 equals 2.expt(10))

  test("test factorial"):
    assert(120 equals factorial(5))
    assert(720 equals factorial(6))

  test("test gcd"):
    assert(12 equals 24.gcd(36))


