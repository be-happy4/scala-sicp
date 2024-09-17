package org.behappy.sicp.lang

import org.behappy.sicp.lang.*
import org.scalatest.funsuite.AnyFunSuite

class TestData extends munit.FunSuite:
  test("test pair"):
    val p = FPair(1, "b")
    assert(1 == p.car)
    assert("b" == p.cdr)
