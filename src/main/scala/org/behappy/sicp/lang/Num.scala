package org.behappy.sicp.lang

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode

opaque type Num = BigDecimal

object Num:
  def apply(d: Double): Num = BigDecimal(d)

  def apply(d: Int): Num = d.toDouble

  def apply(d: Long): Num = BigDecimal(d)

  def apply(d: BigInt): Num = BigDecimal(d)

extension (num: Num)
  def toInt: Int = num.toInt

implicit def int2num(num: Int): Num = Num(num)
implicit def long2num(num: Long): Num = Num(num)
implicit def float2num(num: Double): Num = Num(num)

@targetName("gt")
def >(x: Num, y: Num): Boolean = x > y
@targetName("lt")
def <(x: Num, y: Num): Boolean = x < y
@targetName("ge")
def >=(x: Num, y: Num): Boolean = x >= y
@targetName("le")
def <=(x: Num, y: Num): Boolean = x <= y
@targetName("equals")
def `=`(x: Num, y: Num): Boolean = x == y

@targetName("plus")
def +(x: Num, y: Num): Num = x + y
@targetName("plus")
def +(x: Num, nums: Num*): Num = x + nums.sum // TODO: lisp it
@targetName("minus")
def -(x: Num, y: Num): Num = x - y
@targetName("minus")
def -(x: Num, nums: Num*): Num = x - nums.sum
@targetName("times")
def *(x: Num, y: Num): Num = x * y
@targetName("times")
def *(x: Num, nums: Num*): Num = x * nums.product // TODO: lisp it
@targetName("divide")
def /(x: Num, y: Num): Num = x / y
@targetName("remainder")
def %(x: Num, y: Num): Num = x % y
//  (x + 0.5).toLong % (y + 0.5).toLong

def max(x: Num, y: Num): Num = x.max(y)
def max(x: Num, nums: Num*): Num = nums.fold(x)(_.max(_))
def min(x: Num, y: Num): Num = x.min(y)
def min(x: Num, nums: Num*): Num = nums.fold(x)(_.min(_))
def average(x: Num, y: Num): Num = /(`+`(x, y), 2)
def average(x: Num, nums: Num*): Num = /(`+`(x, nums*), inc(nums.size))

@targetName("negative")
def -(x: Num): Num = -x
def inc(x: Num): Num = `+`(x, 1)
def dec(x: Num): Num = `-`(x, 1)

def abs(x: Num): Num =
  if (>(x, 0)) x
  else `-`(x)

def square(x: Num): Num = *(x, x)
def cube(x: Num): Num = *(x, x, x)

def even(n: Num): Boolean = `=`(%(n, 2), 0)
def odd(n: Num): Boolean = `=`(%(n, 2), 1)

def positive(n: Num): Boolean = >(n, 0)
def negative(n: Num): Boolean = <(n, 0)

def expt(b: Num, n: Num): Num =
  @tailrec
  def fast_expt(b: Num, n: Num, a: Num): Num =
    if (`=`(n, 0)) a
    else if (even(n)) fast_expt(square(b), /(n, 2), a)
    else fast_expt(b, `-`(n, 1), *(b, a))

  fast_expt(b, n, 1)

def divides(a: Num, b: Num): Boolean =
  `=`(%(b, a), 0)

def factorial(n: Num): Num =
  @tailrec
  def factorial_iter(product: Num, counter: Num): Num =
    if (>(counter, n)) product
    else factorial_iter(
      *(counter, product),
      `+`(counter, 1))

  factorial_iter(1, 1)

@tailrec
def gcd(a: Num, b: Num): Num =
  if (`=`(b, 0)) a
  else gcd(b, %(a, b))

def random(x: Num): Num =
  ThreadLocalRandom.current().nextLong(x.toLong)

def random(from: Num, to: Num): Num =
  ThreadLocalRandom.current().nextLong(from.toLong, to.toLong)

def ceiling(x: Num): Num = x.setScale(0, RoundingMode.CEILING)
def flooring(x: Num): Num = x.setScale(0, RoundingMode.FLOOR)

def exact2inexact(x: Num): Num = x // TODO:
def inexact2exact(x: Num): Num = x // TODO:

def sin(x: Num): Num = math.sin(x.toDouble)
def cos(x: Num): Num = math.cos(x.toDouble)
