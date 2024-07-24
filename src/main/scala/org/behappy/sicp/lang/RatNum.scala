package org.behappy.sicp.lang

import org.behappy.sicp.lang
import org.behappy.sicp.lang.FInt.*

import scala.language.implicitConversions

opaque type RatNum = Pair[FInt, FInt]

object RatNum:
  def apply(n: FInt, d: FInt): RatNum =
    val g = n gcd d
    Pair(n / g, d / g)

extension (x: RatNum)
  private def toPair: Pair[FInt, FInt] = x
  
  def numer: FInt = toPair.car

  def denom: FInt = toPair.cdr

  def add_rat(y: RatNum): RatNum =
    RatNum(
      x.numer * y.denom + y.numer * x.denom,
      x.denom * y.denom)

  def sub_rat(y: RatNum): RatNum =
    RatNum(
      x.numer * y.denom - y.numer * x.denom,
      x.denom * y.denom)

  def mul_rat(y: RatNum): RatNum =
    RatNum(
      x.numer * y.numer,
      x.denom * y.denom)

  def div_rat(y: RatNum): RatNum =
    RatNum(
      x.numer * y.denom,
      x.denom * y.numer)

  def eq(y: RatNum): Boolean = x.numer * y.denom == x.denom * y.numer

  def toString: String = f"${x.numer}/${x.denom}"
