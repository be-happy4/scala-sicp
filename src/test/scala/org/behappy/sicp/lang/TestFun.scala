package org.behappy.sicp.lang

import org.scalatest.funsuite.AnyFunSuite

class TestFun extends AnyFunSuite:
  test("test function composite"):
    assert(`=`(21, double[NumFun](double[NumFun](double[Num]))(inc)(5)))
    assert(`=`(49, compose(square, inc)(6)))
    assert(`=`(625, repeated(square, 2)(5)))
