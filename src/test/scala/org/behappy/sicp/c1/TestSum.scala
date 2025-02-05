package org.behappy.sicp.c1

import org.behappy.sicp.c1.Sum.*
import org.behappy.sicp.lang.*

class TestSum extends munit.FunSuite:
  test("test for sum"):
    assert(3025 == sum_cubes(1, 10))
    assert(55 == sum_integers(1, 10))

  test("test for product"):
    assert(720 == product(identify, 1, _ + 1, 6))

