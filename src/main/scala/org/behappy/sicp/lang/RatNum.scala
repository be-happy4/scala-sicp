package org.behappy.sicp.lang

import org.behappy.sicp.lang
import org.behappy.sicp.lang.FInt.*
import org.behappy.sicp.lang.FNum.FNum

import scala.annotation.nowarn
import scala.language.implicitConversions
import scala.math.{ScalaNumber, ScalaNumericConversions}

/**
 * [[BigDecimal]]
 */
class RatNum private(val numer: FInt, val denom: FInt) extends ScalaNumber
  with ScalaNumericConversions
  with Ordered[RatNum]:
  override def isWhole: Boolean = denom != 0
  override def underlying(): (FInt, FInt) = (numer, denom)
  override def intValue(): Int = toFInt.intValue
  override def longValue(): Long = toFInt.longValue
  override def floatValue(): Float = toFNum.floatValue
  override def doubleValue(): Double = toFNum.doubleValue

  def toFInt: FInt = numer / denom
  def toFNum: FNum = BigDecimal(numer) / BigDecimal(denom)

  def +(y: RatNum): RatNum = RatNum(
    numer * y.denom + y.numer * denom,
    denom * y.denom)

  def -(y: RatNum): RatNum = RatNum(
    numer * y.denom - y.numer * denom,
    denom * y.denom)

  def *(y: RatNum): RatNum = RatNum(
    numer * y.numer,
    denom * y.denom)

  def /(y: RatNum): RatNum = RatNum(
    numer * y.denom,
    denom * y.numer)

  infix def equals(that: RatNum): Boolean =
    numer * that.denom == denom * that.numer

  /** Compares this RatNum with the specified value for equality. */
  override infix def equals(that: Any): Boolean = that match
    case that: RatNum     => this equals that
    case that: BigInt     => (denom == 1) && (numer equals that)
//    case that: BigDecimal => that equals this
//    case that: Double     => toDouble == that
//    case that: Float      => toFloat == that
    case x                => unifiedPrimitiveEquals(x)

  override def toString: String = s"$numer/$denom"

  override def compare(that: RatNum): Int =
    (numer * that.denom).compare(that.numer * denom)

object RatNum:
  def apply(numer: FInt, denom: FInt): RatNum =
    require(denom != 0, "Denominator cannot be zero") // Denominator check
    val g = numer gcd denom
    val sign = if (denom < 0) -1 else 1 // Keep the denominator positive
    new RatNum(numer / g * sign, denom / g * sign) // Normalize both numer and denom
