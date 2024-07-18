package org.behappy.sicp.lang

import org.behappy.sicp.lang
import org.behappy.sicp.lang.FInt.*

import scala.language.implicitConversions


object RatNum:
  opaque type RatNum = Pair[FInt, FInt]

  def make_rat(n: FInt, d: FInt): RatNum =
    val g = gcd(n, d)
    cons(/(n, g), /(d, g))

  def numer(x: RatNum): FInt =
    car(x)

  def denom(x: RatNum): FInt =
    cdr(x)

  def add_rat(x: RatNum, y: RatNum): RatNum =
    make_rat(
      `+`(
        *(numer(x), denom(y)),
        *(numer(y), denom(x))),
      *(denom(x), denom(y)))

  def sub_rat(x: RatNum, y: RatNum): RatNum =
    make_rat(
      `-`(
        *(numer(x), denom(y)),
        *(numer(y), denom(x))),
      *(denom(x), denom(y)))

  def mul_rat(x: RatNum, y: RatNum): RatNum =
    make_rat(
      *(numer(x), numer(y)),
      *(denom(x), denom(y)))

  def div_rat(x: RatNum, y: RatNum): RatNum =
    make_rat(
      *(numer(x), denom(y)),
      *(denom(x), numer(y)))

  def equal_rat(x: RatNum, y: RatNum): Boolean =
    `=`(*(numer(x), denom(y)), *(denom(x), numer(y)))

  def print_rat(x: RatNum): Printer =
    display(numer(x))("/")(denom(x))
