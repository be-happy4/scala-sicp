package org.behappy.sicp.lang.stream

import org.behappy.sicp.lang.FInt.{FInt, int2FInt}
import org.behappy.sicp.lang.stream.FIntStream.{+, `*`, integers, merge}
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.{should, shouldEqual}

import scala.collection.Iterable

class StreamTest extends munit.FunSuite:
  test("test map filter"):
    FStream(1, 2, 3) should contain theSameElementsAs Iterable(1, 2, 3)
    FStream(1, 2, 3).map(_ * 2) should contain theSameElementsAs Iterable(2, 4, 6)
    FStream(1, 2, 3).filter(_ % 2 == 0) should contain theSameElementsAs Iterable(2)

  test("test prime stream".ignore):
    FIntStream.range(10001, 1000000)
      .filter(_.isProbablePrime(100))
      .foreach(println)

  test("test fib"):
    def fibgen(a: BigInt = 0, b: BigInt = 1): FStream[BigInt] =
      FStream(a, fibgen(b, a + b))
    def fibs = fibgen()
    def fibs2: LazyList[FInt] =
      BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

    fibs.take(100) should contain theSameElementsAs fibs2.take(100)

  test("test sieve stream"):
    FIntStream.primes(50) shouldEqual 233

  test("test stream addition"):
    FIntStream.double.take(5) should contain theSameElementsAs Iterable(1, 2, 4, 8, 16)
    def s: FIntStream = FIntStream(1, s + s)
    s.take(5) should contain theSameElementsAs Iterable(1, 2, 4, 8, 16)
    def factorials: FIntStream = FIntStream(1, factorials * integers(2))
    factorials.take(5) should contain theSameElementsAs Iterable(1, 2, 6, 24, 120)
    def partialSum(s: FIntStream): FIntStream = FIntStream(s.head, s.tail + partialSum(s))
    partialSum(integers).take(5) should contain theSameElementsAs Iterable(1, 3, 6, 10, 15)

  test("test merge stream"):
    def S: FIntStream = FIntStream(1,
      integers.map(_ * 2)
        .merge(integers.map(_ * 3))
        .merge(integers.map(_ * 5)))
    S.take(10).foreach(println)
