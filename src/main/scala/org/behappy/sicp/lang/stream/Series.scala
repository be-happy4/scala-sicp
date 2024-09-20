package org.behappy.sicp.lang.stream

import org.behappy.sicp.lang.stream.FNumStream.*
import org.behappy.sicp.lang.FNum.{FNum, square}

object Series:
  def expSeries: FNumStream = FNumStream(1,
    expSeries.integrateSeries)
  def sineSeries: FNumStream = FNumStream(0,
    cosineSeries.integrateSeries)
  def cosineSeries: FNumStream = FNumStream(1,
    sineSeries.integrateSeries.map(-_))

  private def piSummands(n: FNum): FNumStream =
    FNumStream(1 / n, piSummands(n + 2).map(-_))

  def piStream: FNumStream =
    piSummands(1).partialSum.map(_ * 4)

  def eulerTransform(s: FNumStream): FNumStream =
    val (s0, s1, s2) = (s.head, s(1), s(2))
    FNumStream(s2 - (s2 - s1).square / (s0 - 2 * s1 + s2),
      eulerTransform(s.tail))

  extension (s: FNumStream)
    def integrateSeries: FNumStream =
      s * (FNumStream.ones / FNumStream.integers)

//    infix def **(s2: FNumStream): FNumStream =
//      FNumStream(s.head * s2.head,
//        s2.tail.map(_ * s.head) + (s ** s2.tail))

    infix def **(s2: FNumStream): FNumStream =
      FNumStream(s.head * s2.head,
        s.tail.map(_ * s2.head) +
          s2.tail.map(_ * s.head) +
          FNumStream(0, s ** s2.tail))

    def makeTableau(transform: FNumStream => FNumStream): FStream[FNumStream] =
      FStream(s, transform(s.tail).makeTableau(transform))

    def acceleratedSequence(transform: FNumStream => FNumStream): FNumStream =
      s.makeTableau(transform).map(_.head)
