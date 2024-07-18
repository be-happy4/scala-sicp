package org.behappy.sicp.lang

import org.behappy.sicp.lang.*
import org.scalatest.funsuite.AnyFunSuite

class TestData extends AnyFunSuite:
  test("test pair"):
    val p = cons(1, "b")
    assert(`=`(1, car(p)))
    assert(`=`("b", cdr(p)))
