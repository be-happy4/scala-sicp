package org.behappy.sicp

import scala.collection.mutable

object Test:
  def main(args: Array[String]): Unit =
    Some(1)
      .getOrElse(1)
    List().map(_.toString).foreach(println)
    None.isEmpty
    Seq().zip(Seq())
  
  