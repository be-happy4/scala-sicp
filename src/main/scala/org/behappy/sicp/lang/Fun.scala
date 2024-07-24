package org.behappy.sicp.lang

import org.behappy.sicp.lang.FInt.FInt
import org.behappy.sicp.lang.FNum.*
import org.behappy.sicp.lang.int2FNum

import scala.annotation.{tailrec, targetName}

type Pre1[T] = T => Boolean
type Pre2[T1, T2] = (T1, T2) => Boolean
type Con1[T] = T => Unit
type Con2[T1, T2] = (T1, T2) => Unit
type Fun1[T, R] = T => R
type Fun2[T1, T2, R] = (T1, T2) => R
type Sup[R] = () => R
type Op1[T] = Fun1[T, T]
type Op2[T] = Fun2[T, T, T]

def identify[T](x: T): T = x

def error(msg: String): Nothing = throw RuntimeException(msg)
def error(msg: String, args: Any*): Nothing =
  println(args.zipWithIndex.map((i, v) => f"$i=>$v").mkString("[", ",", "]"))
  throw RuntimeException(msg)

def double[T](f: Op1[T]): Op1[T] =
  x => f(f(x))

def compose[T](f: Op1[T], g: Op1[T]): Op1[T] =
  x => f(g(x))

def repeated[T](f: Op1[T], n: FInt): Op1[T] =
  @tailrec
  def iter(result: Op1[T] = identify, c: FInt = n): Op1[T] =
    if (c equals 0) result
    else iter(compose(f, result), c.dec)

  iter()