package org.behappy.sicp.c1

import org.behappy.sicp.c1.Ackermann.*
import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FInt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

def to(start: FInt, end: FInt): Range =
  Range(start.toInt, end.toInt)

def test_range(f1: FInt => FInt, f2: FInt => FInt, range: Range = to(1, 10)) =
  for i <- range do
    assert(f1(i) == f2(i))

/**
 * @see [[https://en.wikipedia.org/wiki/Tetration]]
 */
def tetration(b: FInt, n: FInt): FInt =
  @tailrec
  def tetration_iter(count: FInt, s: FInt): FInt =
    if (count equals 0) s
    else tetration_iter(count - 1, b pow s.intValue)

  tetration_iter(n, 1)

class TestAckermann extends munit.FunSuite:
  test("test A"):
    assert(1024 == A(1, 10))
    assert(65536 == A(2, 4))
    assert(65536 == A(3, 3))

  test("test exponent tower"):
    assert(4 == tetration(2, 2))
    assert(16 == tetration(2, 3))
    assert(65536 == tetration(2, 4))

  test("test extent A"):
    test_range(f, x => x * 2)
    test_range(g, x => FInt(2) pow x.toInt)
    test_range(h, x => tetration(2, x), to(1, 4))


