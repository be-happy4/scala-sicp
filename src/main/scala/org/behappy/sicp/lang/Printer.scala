package org.behappy.sicp.lang

trait Printer:
  def apply(o: Any): Printer

  def apply(): String

  def print(): Unit

private class PrinterImpl(o: Any) extends Printer:
  private val str: String = o.toString

  def apply(o: Any): PrinterImpl =
    PrinterImpl(str + o)

  def apply(): String = str

  def print(): Unit = println(str)


def newline(): Printer = PrinterImpl("\n")
def display(o: Any): Printer = PrinterImpl(o)

