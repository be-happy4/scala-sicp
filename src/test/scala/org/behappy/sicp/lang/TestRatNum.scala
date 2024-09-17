package org.behappy.sicp.lang

import org.behappy.sicp.lang.RatNum.*
import org.scalatest.funsuite.AnyFunSuite

class TestRatNum extends munit.FunSuite:
  test("test rational number calculate"):
    assert(RatNum(5, 6) equals RatNum(1, 2) + RatNum(1, 3))
    assert(RatNum(1, 6) equals RatNum(1, 2) - RatNum(1, 3))
    assert(RatNum(1, 6) equals RatNum(1, 2) * RatNum(1, 3))
    assert(RatNum(3, 2) equals RatNum(1, 2) / RatNum(1, 3))

  test("test format"):
    assert("1/2" == RatNum(1, 2).toString)
    assert("1/2" == RatNum(2, 4).toString)

  test("test gcd"):
    assert(RatNum(2, 3) equals RatNum(1, 3) + RatNum(1, 3))
