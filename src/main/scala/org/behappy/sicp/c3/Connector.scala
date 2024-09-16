package org.behappy.sicp.c3

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

case class Connector[T: Numeric]():
  var value: T = _
  var informant: Option[Constraint] = None
  val constraints: mutable.Set[Constraint] = mutable.HashSet[Constraint]()

  def hasValue: Boolean = informant.isDefined

  def getValue: T = value

  def setValue(newValue: T, informant: Constraint): Unit =
    if !hasValue then
      this.value = newValue
      this.informant = Some(informant)
      constraints.foreach(a => if a != informant then
        a.processNewValue())
    else if this.value != newValue then
      throw new IllegalStateException(s"Contradiction $value, $newValue")

  def forgetValue(retractor: Constraint): Unit =
    if retractor == informant then
      informant = None
      constraints.foreach(a => if a != retractor then a.processForgetValue())
  
  def connect(newConstraint: Constraint): Unit =
    constraints += newConstraint
    if hasValue then
      newConstraint.processNewValue()
      



