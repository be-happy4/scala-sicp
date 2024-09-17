package org.behappy.sicp.lang.stream

import org.behappy.sicp.lang.FInt.{FInt, square}

import scala.annotation.tailrec

type FIntStream = FStream[FInt]

object FIntStream:
  def apply(car: FInt, cdr: => FIntStream = FStream.empty): FIntStream =
    FStream(car, cdr)

  def range(from: FInt, to: FInt): FIntStream =
    if from == to then FStream.empty
    else
      FIntStream(from, range(from + 1, to))

  def integers: FIntStream = integers(1)
  def integers(n: FInt): FIntStream =
    FStream(n, integers(n + 1))

  def ones: FIntStream = FIntStream(1, ones)

  def double: FIntStream =
    FIntStream(1, double.map(_ * 2))

  def sieve(stream: FIntStream): FIntStream =
    FStream(stream.head,
      sieve(stream.tail.filter(x => x % stream.head != 0)))

  def primes: FIntStream = sieve(FIntStream.integers(2))

  def isPrime(n: FInt): Boolean =
    @tailrec
    def iter(ps: FIntStream): Boolean =
      if ps.head.square > n then true
      else if n % ps.head == 0 then false
      else iter(ps.tail)
    iter(primes)


  extension (x: FIntStream)
    def +(that: FIntStream): FIntStream =
      x.zip(that).map(_ + _)
    def *(that: FIntStream): FIntStream =
      x.zip(that).map(_ * _)
    def merge(y: FIntStream): FIntStream =
      if x.isEmpty then y
      else if y.isEmpty then x
      else
        val (headX, headY) = (x.head, y.head)
        if headX < headY then FIntStream(headX, x.tail.merge(y))
        else if headX > headY then FIntStream(headY, y.tail.merge(x))
        else FIntStream(headX, x.tail.merge(y.tail))
