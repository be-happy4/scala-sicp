package org.behappy.sicp.c1

import org.behappy.sicp.c1.Ackermann.*
import org.behappy.sicp.lang.*
import org.behappy.sicp.lang.FInt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

def to(start: FInt, end: FInt): Range =
  Range(start.value.toInt, end.value.toInt)

def test_range(f1: FInt => FInt, f2: FInt => FInt, range: Range = to(1, 10)) =
  for i <- range do
    assert(f1(i) equals f2(i))

/**
 * @see [[https://en.wikipedia.org/wiki/Tetration]]
 */
def tetration(b: FInt, n: FInt): FInt =
  @tailrec
  def tetration_iter(count: FInt, s: FInt): FInt =
    if (count equals 0) s
    else tetration_iter(count.dec, b expt s)

  tetration_iter(n, 1)

class TestAckermann extends AnyFunSuite:
  test("test A"):
    assert(int2FInt(1024) equals A(1, 10))
    assert(65536 equals A(2, 4))
    assert(65536 equals A(3, 3))

  test("test exponent tower"):
    assert(4 equals tetration(2, 2))
    assert(16 equals tetration(2, 3))
    assert(65536 equals tetration(2, 4))

  test("test extent A"):
    test_range(f, x => x * 2)
    test_range(g, x => 2 expt x)
    test_range(h, x => tetration(2, x), to(1, 4))


