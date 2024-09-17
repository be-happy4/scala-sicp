package org.behappy.sicp.lang.stream

object FStream:
  def apply[A](car: A, cdr: => FStream[A] = LazyList.empty): FStream[A] =
    car #:: cdr
//    scala.collection.immutable.LazyList.cons(car, cdr)

  def apply[A](param: A*): FStream[A] =
    LazyList(param *)

  def delay[A](value: => A): () => A =
    var already: Option[A] = None
    () =>
      if already.isEmpty then
        already = Some(value)
      already.get

  def empty[A]: FStream[A] = LazyList.empty[A]
/**
 * {{{
 * trait FStream[+A] extends FList[A]
 *   with FListOps[A, FStream, FStream[A]]
 *   with IterableFactoryDefaults[A, FStream]:
 *   override def head: A = car
 *   override def tail: FStream[A] = cdr
 *   override final def isEmpty: Boolean = this eq StreamNone
 *   final def isDefined: Boolean = !isEmpty
 *
 *   override def filter(p: A => Boolean): FStream[A] = this match
 *     case StreamNone => StreamNone
 *     case _ =>
 *       if p(head) then
 *         FStream(head, tail.filter(p))
 *       else tail.filter(p)
 *
 *   override def map[B](f: A => B): FStream[B] = this match
 *     case StreamNone => StreamNone
 *     case _ => FStream(f(head), tail.map(f))
 *
 *   override def iterator: Iterator[A] = new AbstractIterator[A] {
 *     private var current = FStream.this
 *     def hasNext: Boolean = !current.isEmpty
 *     def next(): A = { val r = current.head; current = current.tail; r }
 *   }
 *
 *   override def length: Int =
 *     @tailrec
 *     def _helper(res: Int = 0, stream: FStream[?] = this): Int = stream match
 *       case StreamNone => res
 *       case _ => _helper(res + 1, stream.tail)
 *     _helper()
 *
 *   def apply(i: Int): A =
  *     if i < 0 then throw new IndexOutOfBoundsException(s"Index out of bounds: $i")
 *     @tailrec
 *     def _helper(ni: Int = i, stream: FStream[A] = this): A = stream match
 *       case StreamNone => throw new IndexOutOfBoundsException(s"Index out of bounds: $i")
 *       case _ =>
 *         if ni == 0 then stream.car
 *         else _helper(ni - 1, stream.tail)
 *     _helper()
 *
 *   override def reverse: FStream[A] =
 *     @tailrec
 *     def _helper(stream: FStream[A] = this, acc: FStream[A] = StreamNone): FStream[A] =
 *       if (stream.isEmpty) acc
 *       else _helper(stream.cdr, FStream(stream.car, acc))
 *     _helper()
 *
 *   override def iterableFactory: SeqFactory[FStream] = FStream
 *
 * case class StreamSome[+A](
 *   override val car: A,
 *   private val next: () => FStream[A] = () => StreamNone,
 * ) extends FStream[A]:
 *   override def cdr: FStream[A] = next()
 *
 * case object StreamNone extends FStream[Nothing]:
 *   override def car: Nothing = throw new NoSuchElementException()
 *   override def cdr: FStream[Nothing] = throw new NoSuchElementException()
 * }}}
 */
type FStream[A] = LazyList[A]

