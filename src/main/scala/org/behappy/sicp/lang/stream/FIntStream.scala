package org.behappy.sicp.lang.stream

type FIntStream = FStream[BigInt]

object FIntStream:
  def apply(car: BigInt, cdr: => FIntStream = StreamNone): FIntStream =
    FStream(car, cdr)

  def range(from: BigInt, to: BigInt): FIntStream =
    if from == to then StreamNone
    else
      FIntStream(from, range(from + 1, to))

  def integers(n: BigInt = 1): FIntStream =
    FStream(n, integers(n + 1))

  def ones: FIntStream = FIntStream(1, ones)
  
  def double: FIntStream =
    FIntStream(1, double.map(_ * 2))

  extension(x: FIntStream)
    def +(that: FIntStream): FIntStream =
      x.zip(that).map(_ + _)
