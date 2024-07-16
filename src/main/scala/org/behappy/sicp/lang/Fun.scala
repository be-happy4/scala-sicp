package org.behappy.sicp.lang


def identify[T](t: T): T = t

def error(msg: String): Nothing = throw RuntimeException(msg)
def error(msg: String, args: Any*): Nothing =
  println(args.zipWithIndex.map((i, v) => f"$i=>$v").mkString("[", ",", "]"))
  throw RuntimeException(msg)

