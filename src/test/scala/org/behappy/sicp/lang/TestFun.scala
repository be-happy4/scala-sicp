package org.behappy.sicp.lang

import org.behappy.sicp.lang.FNum.*
import org.scalatest.funsuite.AnyFunSuite

class TestFun extends AnyFunSuite:
  test("test function composite"):
    assert(21 equals double[NumOp1](double[NumOp1](double[FNum]))(inc)(5))
    assert(49 equals compose(square, inc)(6))
    assert(625 equals repeated(square, 2)(5))
