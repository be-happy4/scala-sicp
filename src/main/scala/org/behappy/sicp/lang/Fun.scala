package org.behappy.sicp.lang

import scala.annotation.tailrec

type UnaryOp[T] = T => T
type Pre[T] = T => Boolean
type BiPre[T1, T2] = (T1, T2) => Boolean

def identify[T](x: T): T = x

def error(msg: String): Nothing = throw RuntimeException(msg)
def error(msg: String, args: Any*): Nothing =
  println(args.zipWithIndex.map((i, v) => f"$i=>$v").mkString("[", ",", "]"))
  throw RuntimeException(msg)


def double[T](f: UnaryOp[T]): UnaryOp[T] =
  x => f(f(x))

def compose[T](f: UnaryOp[T], g: UnaryOp[T]): UnaryOp[T] =
  x => f(g(x))

def repeated[T](f: UnaryOp[T], n: Num): UnaryOp[T] =
  @tailrec
  def iter(result: UnaryOp[T] = identify, c: Num = n): UnaryOp[T] =
    if (`=`(c, 0)) result
    else iter(compose(f, result), dec(c))

  iter()