package org.behappy.sicp.c3

case class Probe[T: Numeric](
  val name: String,
  val connector: Connector[T]) extends Constraint:
  def printProbe(value: Any): Unit =
    println()
    print(name)
    print(" = ")
    print(value)
    
  def processNewValue(): Unit =
    printProbe(connector.getValue)
    
  def processForgetValue(): Unit =
    printProbe("?")
  
  connector.connect(this)
