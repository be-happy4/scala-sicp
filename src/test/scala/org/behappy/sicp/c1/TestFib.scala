package org.behappy.sicp.c1

import org.behappy.sicp.c1.Fib.*

class TestFib extends munit.FunSuite:
  test("test fib"):
    assert(5 == fib_tree(5))
    assert(8 == fib_tree(6))
    assert(21 == fib_tree(8))

    assert(5 == fib_matrix(5))
    assert(8 == fib_matrix(6))
    assert(21 == fib_matrix(8))

