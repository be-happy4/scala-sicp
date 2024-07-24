package org.behappy.sicp.lang

import org.behappy.sicp.lang.FInt
import org.behappy.sicp.lang.FInt.FInt
import org.behappy.sicp.lang.FNum.FNum

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode


type NumOp1 = Op1[FNum]
type NumOp2 = Op2[FNum]
type NumPre1 = Pre1[FNum]
type NumPre2 = Pre2[FNum, FNum]

implicit def int2FNum(num: Int): FNum = FNum(num)

implicit def long2FNum(num: Long): FNum = FNum(num)

implicit def float2FNum(num: Double): FNum = FNum(num)

object FNum:
  opaque type FNum = BigDecimal

  def random(x: FNum = Long.MaxValue): FNum =
    ThreadLocalRandom.current().nextLong(x.toLong)

  def random(from: FNum, to: FNum): FNum =
    ThreadLocalRandom.current().nextLong(from.toLong, to.toLong)

  def apply(d: Number): FNum =
    d match
      case l: Long => BigDecimal(l)
      case l: BigInt => BigDecimal(l)
      case l: BigDecimal => l
      case _ => BigDecimal(d.doubleValue())

  def E: FNum = math.E

  def PI: FNum = math.Pi

  extension (x: FNum)
    def value: BigDecimal = x

    def toFInt: FInt = x.toBigInt

    @targetName("gt")
    infix def >(y: FNum): Boolean = x > y

    @targetName("lt")
    infix def <(y: FNum): Boolean = x < y

    @targetName("gteq")
    infix def >=(y: FNum): Boolean = x >= y

    @targetName("le")
    infix def <=(y: FNum): Boolean = x <= y

    def equals(obj: Any): Boolean =
      obj match
        case n: FNum => x == n
        case n: Number => x == n
        case _ => false

    def hashCode(): Int = x.hashCode()
    
    def ne(obj: Any): Boolean = !equals(obj)

    @targetName("plus")
    infix def +(y: FNum): FNum = x + y

    @targetName("plus")
    infix def +(nums: FNum*): FNum = x + nums.sum // TODO: lisp it

    @targetName("minus")
    infix def -(y: FNum): FNum = x - y

    @targetName("minus")
    infix def -(nums: FNum*): FNum = x - nums.sum

    @targetName("times")
    infix def *(y: FNum): FNum = x * y

    @targetName("times")
    infix def *(nums: FNum*): FNum = x * nums.product // TODO: lisp it

    @targetName("divide")
    infix def /(y: FNum): FNum = x / y

    @targetName("remainder")
    infix def %(y: FNum): FNum = x % y
    //  (x + 0.5).toLong % (y + 0.5).toLong

    def max(y: FNum): FNum = x max y

    def max(nums: FNum*): FNum = nums.fold(x)(_ max _)

    def min(y: FNum): FNum = x.min(y)

    def min(nums: FNum*): FNum = nums.fold(x)(_ min _)

    def average(y: FNum): FNum = (x + y) / 2

    def average(nums: FNum*): FNum = x.+(nums: _*) / (nums.size + 1)

    @targetName("negative")
    def unary_-(): FNum = -x

    def inc: FNum = x + 1

    def dec: FNum = x - 1

    def abs: FNum = x.abs

    def square: FNum = x * x

    def cube: FNum = x * x * x

    def isEven: Boolean = x % 2 == 0

    def isOdd: Boolean = !isEven

    def isPositive: Boolean = x > 0

    def isNegative: Boolean = x < 0

    def expt(n: FNum): FNum =
      @tailrec
      def fast_expt(b: FNum, n: FNum, a: FNum): FNum =
        if (n == 0) a
        else if (isEven) fast_expt(b.square, n / 2, a)
        else fast_expt(b, n.dec, b * a)

      fast_expt(x, n, 1)

    def log2: FNum =
      log(2)

    def log(y: FNum = E): FNum =
      // TODO: lisp it. precise it
      math.log(x.toDouble) / math.log(y.toDouble)

    def divides(y: FNum): Boolean = x % y == 0

    def factorial: FNum =
      @tailrec
      def factorial_iter(product: FNum, counter: FNum): FNum =
        if (counter > x) product
        else factorial_iter(counter * product, counter.inc)

      factorial_iter(1, 1)

    @tailrec
    def gcd(y: FNum): FNum =
      if (y == 0) x
      else y.gcd(x % y)

    def ceiling: FNum = x.setScale(0, RoundingMode.CEILING)

    def flooring: FNum = x.setScale(0, RoundingMode.FLOOR)

    def exact2inexact: FNum = x // TODO:

    def inexact2exact: FNum = x // TODO:

    def sin: FNum = math.sin(x.toDouble)

    def cos: FNum = math.cos(x.toDouble)
