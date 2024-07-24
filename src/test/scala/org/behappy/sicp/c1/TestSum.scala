package org.behappy.sicp.c1

import org.behappy.sicp.c1.Sum.*
import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FNum.inc
import org.scalatest.funsuite.AnyFunSuite

class TestSum extends AnyFunSuite:
  test("test for sum"):
    assert(3025 eq sum_cubes(1, 10).asInstanceOf)
    assert(55 eq sum_integers(1, 10).asInstanceOf)

  test("test for product"):
    assert(720 eq product(identify, 1, inc, 6).asInstanceOf)

