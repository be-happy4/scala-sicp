package org.behappy.sicp.lang

import org.behappy.sicp.lang.RatNum.{RatNum, denom, make_rat, numer}

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode


object FInt:
  opaque type FInt = BigInt

  def apply(d: Int): FInt = BigInt(d)

  def apply(d: Long): FInt = BigInt(d)

  def apply(d: BigInt): FInt = d

  implicit def int2FInt(num: Int): FInt = FInt(num)

  implicit def long2FInt(num: Long): FInt = FInt(num)

  implicit def float2FInt(num: BigInt): FInt = FInt(num)

  implicit def rat2int(rat: RatNum): FInt =
    if (`=`(0, denom(rat))) numer(rat)
    else error("denominator not 0")

  @targetName("gt")
  def >(x: FInt, y: FInt): Boolean = x > y

  @targetName("lt")
  def <(x: FInt, y: FInt): Boolean = x < y

  @targetName("ge")
  def >=(x: FInt, y: FInt): Boolean = x >= y

  @targetName("le")
  def <=(x: FInt, y: FInt): Boolean = x <= y

  @targetName("plus")
  def +(x: FInt, y: FInt): FInt = x + y

  @targetName("plus")
  def +(x: FInt, nums: FInt*): FInt = x + nums.sum // TODO: lisp it

  @targetName("minus")
  def -(x: FInt, y: FInt): FInt = x - y

  @targetName("minus")
  def -(x: FInt, nums: FInt*): FInt = x - nums.sum

  @targetName("times")
  def *(x: FInt, y: FInt): FInt = x * y

  @targetName("times")
  def *(x: FInt, nums: FInt*): FInt = x * nums.product // TODO: lisp it

  @targetName("divide")
  def /(x: FInt, y: FInt): FInt = x / y

  @targetName("remainder")
  def %(x: FInt, y: FInt): FInt = x % y

  def max(x: FInt, y: FInt): FInt = x.max(y)

  def max(x: FInt, nums: FInt*): FInt = nums.fold(x)(_.max(_))

  def min(x: FInt, y: FInt): FInt = x.min(y)

  def min(x: FInt, nums: FInt*): FInt = nums.fold(x)(_.min(_))

  def average(x: FInt, y: FInt): FInt = /(`+`(x, y), 2)

  def average(x: FInt, nums: FInt*): FInt = /(`+`(x, nums *), inc(nums.size))

  @targetName("negative")
  def -(x: FInt): FInt = -x

  def inc(x: FInt): FInt = `+`(x, 1)

  def dec(x: FInt): FInt = `-`(x, 1)

  def abs(x: FInt): FInt =
    if (>(x, 0)) x
    else `-`(x)

  def square(x: FInt): FInt = *(x, x)

  def cube(x: FInt): FInt = *(x, x, x)

  def even(n: FInt): Boolean = `=`(%(n, 2), 0)

  def odd(n: FInt): Boolean = `=`(%(n, 2), 1)

  def positive(n: FInt): Boolean = >(n, 0)

  def negative(n: FInt): Boolean = <(n, 0)

  def expt(b: FInt, n: FInt): FInt =
    @tailrec
    def fast_expt(b: FInt, n: FInt, a: FInt): FInt =
      if (`=`(n, 0)) a
      else if (even(n)) fast_expt(square(b), /(n, 2), a)
      else fast_expt(b, `-`(n, 1), *(b, a))

    fast_expt(b, n, 1)

  def divides(a: FInt, b: FInt): Boolean =
    `=`(%(b, a), 0)

  @tailrec
  def gcd(a: FInt, b: FInt): FInt =
    if (`=`(b, 0)) a
    else gcd(b, %(a, b))

  def random(x: FInt): FInt =
    ThreadLocalRandom.current().nextLong(x.toLong)

  def random(from: FInt, to: FInt): FInt =
    ThreadLocalRandom.current().nextLong(from.toLong, to.toLong)

