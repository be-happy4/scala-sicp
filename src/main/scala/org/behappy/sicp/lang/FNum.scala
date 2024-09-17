package org.behappy.sicp.lang

import org.behappy.sicp.lang.FInt.FInt
import org.behappy.sicp.lang.FNum.FNum

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode


type NumOp1 = Op1[FNum]
type NumOp2 = Op2[FNum]
type NumPre1 = Pre1[FNum]
type NumPre2 = Pre2[FNum, FNum]


object FNum:
  type FNum = BigDecimal

  implicit def int2FNum: Int => FNum = BigDecimal.int2bigDecimal
  implicit def long2FNum: Long => FNum = BigDecimal.long2bigDecimal
  implicit def double2FNum: Double => FNum = BigDecimal.double2bigDecimal
  implicit def FInt2FNum: FInt => FNum = apply

  def random(): FNum = ThreadLocalRandom.current().nextDouble()
  def random(bound: FNum): FNum =
    ThreadLocalRandom.current().nextDouble(bound.doubleValue)
  def random(origin: FNum, bound: FNum): FNum =
    ThreadLocalRandom.current().nextDouble(origin.doubleValue, bound.doubleValue)

  def apply(d: Number): FNum =
    d match
      case l: Long => BigDecimal(l)
      case l: BigInt => BigDecimal(l)
      case l: BigDecimal => l
      case _ => BigDecimal(d.doubleValue())

  final val E: FNum = math.E

  final val PI: FNum = math.Pi

  extension (x: FNum)
    def toFInt: FInt = x.toBigInt

    def average(y: FNum): FNum = (x + y) / 2

    def average(nums: FNum*): FNum = nums.foldLeft(x)(_ + _) / (nums.size + 1)
    def inc: FNum = x + 1
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
        else if (n.isEven) fast_expt(b.square, n / 2, a)
        else fast_expt(b, n - 1, b * a)
      fast_expt(x, n, 1)

    def log2: FNum =
      log(2)

    def log(y: FNum = E): FNum =
      math.log(x.toDouble) / math.log(y.toDouble)

    infix def divides(y: FNum): Boolean = x % y == 0

    def factorial: FNum =
      @tailrec
      def factorial_iter(product: FNum, counter: FNum): FNum =
        if (counter > x) product
        else factorial_iter(counter * product, counter + 1)

      factorial_iter(1, 1)

    infix def gcd(y: FNum): FNum = apply(x.toBigInt gcd y.toBigInt)

    def ceiling: FNum = x.setScale(0, RoundingMode.CEILING)

    def flooring: FNum = x.setScale(0, RoundingMode.FLOOR)

    def sin: FNum = math.sin(x.toDouble)

    def cos: FNum = math.cos(x.toDouble)
