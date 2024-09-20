package org.behappy.sicp.lang.stream

import org.behappy.sicp.lang.FNum.{FNum, square}

import scala.annotation.tailrec

type FNumStream = FStream[FNum]

object FNumStream:
  def apply(car: FNum, cdr: => FNumStream = FStream.empty): FNumStream =
    FStream(car, cdr)

  def range(from: FNum, to: FNum): FNumStream =
    if from == to then FStream.empty
    else
      FNumStream(from, range(from + 1, to))

  def integers: FNumStream = integers(1)
  def integers(n: FNum): FNumStream =
    FStream(n, integers(n + 1))

  def ones: FNumStream = FNumStream(1, ones)

  def double: FNumStream =
    FNumStream(1, double.map(_ * 2))

  def sieve(stream: FNumStream): FNumStream =
    FStream(stream.head,
      sieve(stream.tail.filter(x => x % stream.head != 0)))

  def primes: FNumStream = sieve(FNumStream.integers(2))

  def isPrime(n: FNum): Boolean =
    @tailrec
    def iter(ps: FNumStream): Boolean =
      if ps.head.square > n then true
      else if n % ps.head == 0 then false
      else iter(ps.tail)
    iter(primes)


  extension (x: FNumStream)
    def +(that: FNumStream): FNumStream =
      x.zip(that).map(_ + _)
    def *(that: FNumStream): FNumStream =
      x.zip(that).map(_ * _)
    def /(that: FNumStream): FNumStream =
      x.zip(that).map(_ / _)
      
    def merge(y: FNumStream): FNumStream =
      if x.isEmpty then y
      else if y.isEmpty then x
      else
        val (headX, headY) = (x.head, y.head)
        if headX < headY then FNumStream(headX, x.tail.merge(y))
        else if headX > headY then FNumStream(headY, y.tail.merge(x))
        else FNumStream(headX, x.tail.merge(y.tail))

    def partialSum: FNumStream =
      def ps: FNumStream = x + FNumStream(0, ps)
      ps