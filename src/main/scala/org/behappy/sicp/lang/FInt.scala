package org.behappy.sicp.lang


import org.behappy.sicp.lang.FNum.FNum

import java.util.concurrent.ThreadLocalRandom
import scala.language.implicitConversions

object FInt:
  type FInt = BigInt

  implicit def int2FInt: Int => FInt = BigInt.int2bigInt
  implicit def long2FInt: Long => FInt = BigInt.long2bigInt

  def apply(d: Number): FInt =
    d match
      case l: BigInt => l
      case _ => BigInt(d.longValue())

  def random(bound: Long = Long.MaxValue): FInt =
    ThreadLocalRandom.current().nextLong(bound)

  def random(origin: FInt, bound: FInt): FInt =
    ThreadLocalRandom.current().nextLong(origin.toLong, bound.toLong)

  extension (x: FInt)
    def square: FInt = x * x
    def cube: FInt = x * x * x
    def isEven: Boolean = x % 2 == 0
    def isOdd: Boolean = !isEven
    def isPositive: Boolean = x > 0
    def isNegative: Boolean = x < 0

