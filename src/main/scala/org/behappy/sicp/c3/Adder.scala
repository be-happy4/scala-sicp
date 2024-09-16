package org.behappy.sicp.c3

import scala.math.Numeric.Implicits.infixNumericOps

case class Adder[T: Numeric](
  val a1: Connector[T], val a2: Connector[T], val sum: Connector[T])
  extends Constraint:
  override def processNewValue(): Unit =
    if a1.hasValue && a2.hasValue then
      sum.setValue(a1.getValue + a2.getValue, this)
    else if a1.hasValue && sum.hasValue then
      a2.setValue(sum.getValue - a1.getValue, this)
    else if a2.hasValue && sum.hasValue then
      a1.setValue(sum.getValue - a2.getValue, this)

  override def processForgetValue(): Unit =
    sum.forgetValue(this)
    a1.forgetValue(this)
    a2.forgetValue(this)
    processNewValue()

  a1.connect(this)
  a2.connect(this)
  sum.connect(this)
  
