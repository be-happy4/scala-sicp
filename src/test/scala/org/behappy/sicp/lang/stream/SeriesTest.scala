package org.behappy.sicp.lang.stream;

import org.behappy.sicp.lang.stream.FNumStream.+
import org.behappy.sicp.lang.stream.Series.{cosineSeries, **, sineSeries, acceleratedSequence}
import org.scalatest.matchers.should.Matchers.{should, shouldEqual}


class SeriesTest extends munit.FunSuite:
  test("test series"):
    def circleSeries: FNumStream =
      cosineSeries.**(cosineSeries) +
        sineSeries.**(sineSeries)

    circleSeries.take(300).foreach(println)
//    println(circleSeries.take(300).sum)

  test("test pi series"):
    Series.piStream.take(8).foreach(println)
    println("\neuler transform: ")
    Series.eulerTransform(Series.piStream).take(20).foreach(println)
    println("\naccelerated euler transform: ")
    (Series.piStream).acceleratedSequence(Series.eulerTransform).take(10).foreach(println)
