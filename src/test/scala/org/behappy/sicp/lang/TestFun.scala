package org.behappy.sicp.lang

import org.behappy.sicp.lang.FNum.*
import org.scalatest.funsuite.AnyFunSuite

class TestFun extends munit.FunSuite:
  test("test function composite"):
    assert(21 == double[NumOp1](double[NumOp1](double[FNum]))(_ + 1)(5))
    assert(49 == compose(square, _ + 1)(6))
    assert(625 == repeated(square, 2)(5))
