package org.behappy.sicp.c3

import scala.math.Fractional.Implicits.infixFractionalOps

case class Multiplier[T: Fractional](
  val m1: Connector[T], val m2: Connector[T], val product: Connector[T])
  extends Constraint:
  override def processNewValue(): Unit =
    if m1.hasValue && m2.hasValue then
      product.setValue(m1.getValue * m2.getValue, this)
    else if m1.hasValue && product.hasValue then
      m2.setValue(product.getValue / m1.getValue, this)
    else if m2.hasValue && product.hasValue then
      m1.setValue(product.getValue / m2.getValue, this)

  override def processForgetValue(): Unit =
    product.forgetValue(this)
    m1.forgetValue(this)
    m2.forgetValue(this)
    processNewValue()

  m1.connect(this)
  m2.connect(this)
  product.connect(this)

