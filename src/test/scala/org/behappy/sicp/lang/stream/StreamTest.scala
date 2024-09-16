package org.behappy.sicp.lang.stream

import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.{should, shouldEqual}

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

    fibs.take(10) should contain theSameElementsAs Iterable(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

  test("test sieve stream"):
    def sieve(stream: FIntStream): FIntStream =
      FStream(stream.head,
        sieve(stream.tail.filter(x => x % stream.head != 0)))

    def primes = sieve(FIntStream.integers(2))
    primes(50) shouldEqual 233

  test("test stream addition"):
    FIntStream.double.take(5) should contain theSameElementsAs Iterable(1, 2, 4, 8, 16)
