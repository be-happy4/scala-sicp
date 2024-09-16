package org.behappy.sicp.c3

trait Constraint:
  def processNewValue(): Unit
  
  def processForgetValue(): Unit
  