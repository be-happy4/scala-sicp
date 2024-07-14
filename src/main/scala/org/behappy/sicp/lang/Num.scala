package org.behappy.sicp.lang

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions

opaque type Num = BigDecimal

object Num:
  def apply(d: Double): Num = BigDecimal(d)
  def apply(d: Int): Num = d.toDouble
  def apply(d: Long): Num = BigDecimal(d)
  def apply(d: BigInt): Num = BigDecimal(d)

extension(num: Num)
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
def +(x: Num, nums: Num*): Num =
  // TODO: lisp it
  x + nums.sum
@targetName("minus")
def -(x: Num, y: Num): Num = x - y
@targetName("times")
def *(x: Num, y: Num): Num = x * y
@targetName("times")
def *(x: Num, nums: Num*): Num =
  // TODO: lisp it
  x * nums.product
@targetName("divide")
def /(x: Num, y: Num): Num = x / y
@targetName("remainder")
def %(x: Num, y: Num): Num = x % y
//  (x + 0.5).toLong % (y + 0.5).toLong

@targetName("negative")
def -(x: Num): Num = -x

def abs(x: Num): Num =
  if (>(x, 0)) x
  else `-`(x)

def square(x: Num): Num = x * x

def average(x: Num, y: Num): Num =
  /(`+`(x, y), 2)

def even(n: Num): Boolean =
  `=`(%(n, 2), 0)

def odd(n: Num): Boolean =
  `=`(%(n, 2), 1)

def expt(b: Num, n: Num): Num =
  @tailrec
  def fast_expt(b: Num, n: Num, a: Num): Num =
    if (`=`(n, 0)) a
    else if (even(n)) fast_expt(square(b), /(n, 2), a)
    else fast_expt(b, `-`(n, 1), *(b, a))

  fast_expt(b, n, 1)

def sqrt(x: Num): Num =
  def good_enough(guess: Num, x: Num): Boolean =
    <(abs(`-`(square(guess), x)), 0.001)

  def improve(guess: Num): Num =
    average(guess, /(x, guess))

  @tailrec
  def sqrt_iter(guess: Num): Num =
    if (good_enough(guess, x)) guess
    else sqrt_iter(improve(guess))

  sqrt_iter(1)

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