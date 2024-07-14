package org.behappy.sicp.lang

import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite

class TestNum extends AnyFunSuite:
  test("test ="):
    assert(`=`(1, 1))
    assert(`=`(1, 1))
    assert(!`=`(0, 1))

  test("test +"):
    assert(`=`(1, `+`(1)))
    assert(`=`(3, `+`(1, 2)))
    assert(`=`(15, `+`(1, 2, 3, 4, 5)))

  test("test *"):
    assert(`=`(1, `*`(1)))
    assert(`=`(2, `*`(1, 2)))
    assert(`=`(120, `*`(1, 2, 3, 4, 5)))

  test("test square"):
    assert(`=`(9, square(3)))
    assert(`=`(16, square(4)))
    assert(`=`(121, square(11)))

  test("test expt"):
    assert(`=`(1, expt(2, 0)))
    assert(`=`(128, expt(2, 7)))
    assert(`=`(1024, expt(2, 10)))

  test("test factorial"):
    assert(`=`(120, factorial(5)))
    assert(`=`(720, factorial(6)))

  test("test gcd"):
    assert(`=`(12, gcd(24, 36)))


