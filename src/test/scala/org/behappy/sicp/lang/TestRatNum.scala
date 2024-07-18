package org.behappy.sicp.lang

import org.behappy.sicp.lang.RatNum.*
import org.scalatest.funsuite.AnyFunSuite

class TestRatNum extends AnyFunSuite:
  test("test rational number calculate")
    assert(equal_rat(make_rat(5, 6), add_rat(make_rat(1, 2), make_rat(1, 3))))
    assert(equal_rat(make_rat(1, 6), sub_rat(make_rat(1, 2), make_rat(1, 3))))
    assert(equal_rat(make_rat(1, 6), mul_rat(make_rat(1, 2), make_rat(1, 3))))
    assert(equal_rat(make_rat(3, 2), div_rat(make_rat(1, 2), make_rat(1, 3))))

  test("test format")
    assert("1/2" == print_rat(make_rat(1, 2))())

  test("test gcd"):
    assert(equal_rat(make_rat(2, 3), add_rat(make_rat(1, 3), make_rat(1, 3))))
