package org.behappy.sicp.lang




object FList:
  def apply[A](car: A, cdr: FList[A] = Nil): FList[A] =
    car :: cdr

/**
 * {{{
 * import scala.annotation.tailrec
 * import scala.collection.immutable.StrictOptimizedSeqOps
 * import scala.collection.{AbstractIterator, IterableFactoryDefaults, Seq, SeqFactory, SeqOps,
 *   StrictOptimizedSeqFactory, mutable}
 * trait FList[+A] extends Seq[A]
 *   with FListOps[A, FList, FList[A]]
 *   with StrictOptimizedSeqOps[A, FList, FList[A]]
 *   with IterableFactoryDefaults[A, FList]:
 *   override def iterator: Iterator[A] = new AbstractIterator[A]:
 *     private var current = FList.this
 *     def hasNext: Boolean = current.nonEmpty
 *     def next(): A = { val r = current.head; current = current.tail; r }
 *   override def length: Int =
 *     @tailrec
 *     def _helper(res: Int = 0, list: FList[?] = this): Int = list match
 *       case ListNone => res
 *       case _ => _helper(res + 1, list.tail)
 *     _helper()
 *   def apply(i: Int): A =
 *     if i < 0 then throw new IndexOutOfBoundsException(s"Index out of bounds: $i")
 *     @tailrec
 *     def _helper(ni: Int = i, list: FList[A] = this): A = list match
 *       case ListNone => throw new IndexOutOfBoundsException(s"Index out of bounds: $i"
 *       case _ =>
 *         if ni == 0 then list.car
 *         else _helper(ni - 1, list.tail)
 *     _helper()
 *   override def iterableFactory: SeqFactory[FList] = FList
 *   override def reverse: FList[A] =
 *     @tailrec
 *     def _helper(list: FList[A], acc: FList[A]): FList[A] = list match
 *       case ListNone => acc
 *       case _ => _helper(list.tail, FList(list.head, acc))
 *     _helper(this, FList.empty)
 *   override def concat[B >: A](that: FList[B]): FList[B] =
 *     @tailrec
 *     def _helper(list1: FList[B], list2: FList[B]): FList[B] = list1 match
 *       case ListNone => list2
 *       case ListSome(head, tail) => _helper(tail, FList(head, list2))
 *     _helper(this, that)
 * case class ListSome[+A](
 *   override val car: A,
 *   override val cdr: FList[A] = ListNone,
 * ) extends FList[A]
 * case object ListNone extends FList[Nothing]:
 *   override def car: Nothing = throw new NoSuchElementException()
 *   override def cdr: FList[Nothing] = throw new NoSuchElementException()
 * }}}
 */
type FList[A] = List[A]
