package org.behappy.sicp.c1

import org.behappy.sicp.c1.Fib.*
import org.behappy.sicp.lang.*
import org.scalatest.funsuite.AnyFunSuite

class TestFib extends AnyFunSuite:
  test("test fib"):
    assert(5 eq fib_tree(5).asInstanceOf)
    assert(8 eq fib_tree(6).asInstanceOf)
    assert(21 eq fib_tree(8).asInstanceOf)

    assert(5 eq fib_matrix(5).asInstanceOf)
    assert(8 eq fib_matrix(6).asInstanceOf)
    assert(21 eq fib_matrix(8).asInstanceOf)

