package org.behappy.sicp.lang


import org.behappy.sicp.lang.FInt.FInt

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions


implicit def int2FInt(num: Int): FInt = FInt(num)

implicit def long2FInt(num: Long): FInt = FInt(num)

implicit def float2FInt(num: BigInt): FInt = FInt(num)

implicit def rat2int(rat: RatNum): FInt =
  if (rat.denom equals 0) rat.numer
  else error("denominator not 0")

object FInt:
  opaque type FInt = BigInt

  def apply(d: Number): FInt =
    d match
      case l: BigInt => l
      case _ => BigInt(d.longValue())

  def random(x: FInt = Long.MaxValue): FInt =
    ThreadLocalRandom.current().nextLong(x.toLong)

  def random(from: FInt, to: FInt): FInt =
    ThreadLocalRandom.current().nextLong(from.toLong, to.toLong)

  extension (x: FInt)
    def value: BigInt = x

    @targetName("gt")
    infix def >(y: FInt): Boolean = x > y

    @targetName("lt")
    infix def <(y: FInt): Boolean = x < y

    @targetName("ge")
    infix def >=(y: FInt): Boolean = x >= y

    @targetName("le")
    def <=(y: FInt): Boolean = x <= y

    def equals(obj: Any): Boolean =
      obj match
        case n: FInt => x == n
        case n: Number => x == n
        case _ => false

    def hashCode(): Int = x.hashCode()
    
    def ne(obj: Any): Boolean = !equals(obj)

    @targetName("plus")
    infix def +(y: FInt): FInt = x + y

    @targetName("plus")
    def +(nums: FInt*): FInt = x + nums.sum // TODO: lisp it

    @targetName("minus")
    def -(y: FInt): FInt = x - y

    @targetName("minus")
    def -(nums: FInt*): FInt = x - nums.sum

    @targetName("times")
    def *(y: FInt): FInt = x * y

    @targetName("times")
    def *(nums: FInt*): FInt = x * nums.product // TODO: lisp it

    @targetName("divide")
    def /(y: FInt): FInt = x / y

    @targetName("remainder")
    def %(y: FInt): FInt = x % y

    def max(y: FInt): FInt = x max y

    def max(nums: FInt*): FInt = nums.fold(x)(_ max _)

    def min(y: FInt): FInt = x min y

    def min(nums: FInt*): FInt = nums.fold(x)(_ min _)

    def average(y: FInt): FInt = (x + y) / 2

    def average(nums: FInt*): FInt = x.+(nums: _*) / (nums.length + 1)

    @targetName("negative")
    def `unary_-`: FInt = -x

    def inc: FInt = x + 1

    def dec: FInt = x - 1

    def abs: FInt = x.abs

    def square: FInt = x * x

    def cube: FInt = x * x * x

    def isEven: Boolean = x % 2 == 0

    def isOdd: Boolean = !isEven

    def isPositive: Boolean = x > 0

    def isNegative: Boolean = x < 0

    def expt(n: FInt): FInt =
      @tailrec
      def fast_expt(b: FInt, n: FInt, a: FInt): FInt =
        if (n == 0) a
        else if (isEven) fast_expt(b.square, n / 2, a)
        else fast_expt(b, n.dec, b * a)

      fast_expt(x, n, 1)

    def divides(y: FInt): Boolean = x % y == 0

    def gcd(y: FInt): FInt = x gcd y

