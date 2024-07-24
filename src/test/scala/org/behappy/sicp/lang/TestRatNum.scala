package org.behappy.sicp.lang

import org.behappy.sicp.lang.RatNum.*
import org.scalatest.funsuite.AnyFunSuite

class TestRatNum extends AnyFunSuite:
  test("test rational number calculate")
    assert(RatNum(5, 6) eq RatNum(1, 2).add_rat(RatNum(1, 3)))
    assert(RatNum(1, 6) eq RatNum(1, 2).sub_rat(RatNum(1, 3)))
    assert(RatNum(1, 6) eq RatNum(1, 2).mul_rat(RatNum(1, 3)))
    assert(RatNum(3, 2) eq RatNum(1, 2).div_rat(RatNum(1, 3)))

  test("test format")
    assert("1/2" == RatNum(1, 2).toString)

  test("test gcd"):
    assert(RatNum(2, 3) eq RatNum(1, 3).add_rat(RatNum(1, 3)))
