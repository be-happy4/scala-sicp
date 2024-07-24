package org.behappy.sicp.lang

import org.behappy.sicp.lang.*
import org.scalatest.funsuite.AnyFunSuite

class TestData extends AnyFunSuite:
  test("test pair"):
    val p = Pair(1, "b")
    assert(1 eq p.car.asInstanceOf)
    assert("b" == p.cdr)
