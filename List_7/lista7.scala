
// Rafał Jasiński

//zadanie 1
class MyQueue[+T] private(private val begin: List[T], private val end: List[T]):
  def this() = this(Nil, Nil)

  def isEmpty: Boolean =
    begin == Nil

  def enqueue[A >: T](elem: A): MyQueue[A] =
    begin match
      case Nil => new MyQueue[A](elem :: begin, end)
      case _ => new MyQueue[A](begin, elem :: end)

  def first: T =
    begin match
      case Nil => throw new NoSuchElementException("Empty Queue")
      case h :: _ => h

  def firstOption: Option[T] =
    begin match
      case Nil => None
      case h :: _ => Some(h)

  def dequeue: MyQueue[T] =
    begin match
      case Nil => this
      case _ :: Nil =>
        new MyQueue[T](end.reverse, Nil)
      case _ :: t =>
        new MyQueue[T](t, end)

  override def toString: String = (s"${begin}, ${end}")

private object MyQueue:
  def empty[T]: MyQueue[T] = new MyQueue[T](Nil, Nil)
  def apply[T](xs: T*): MyQueue[T] = new MyQueue(xs.toList, Nil)


// Zadanie 2
enum BT[+A]:
  case Empty
  case Node(elem: A, left: BT[A], right: BT[A])

import BT.*

def breadthBT[T](tree: BT[T]): List[T] =
  def breath_internal(queue: MyQueue[BT[T]], list: List[T]): List[T] =
    queue.firstOption match
      case Some(Node(element, left, right)) => element :: breath_internal(queue.dequeue.enqueue(left).enqueue(right), list)
      case Some(Empty) => breath_internal(queue.dequeue, list)
      case None => Nil

  breath_internal(MyQueue(tree), Nil)


val e1 = "e1"
val e2 = "e2"
val t: BT[Int] = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val tt: BT[Int] = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

object lista7:
  def main(args: Array[String]): Unit =
    println((new MyQueue).toString)
    println(MyQueue().toString)
    println(MyQueue.empty.enqueue('a').toString)
    println(MyQueue('a', 'b', 'c').toString)
    println(MyQueue('a', 'b', 'c').enqueue('d').enqueue('e').toString)
    println(MyQueue('a', 'b', 'c').dequeue.toString)

    println(MyQueue.empty.enqueue(e1).isEmpty)
    println(MyQueue.empty.isEmpty)
    println(MyQueue.empty.dequeue)
    println(MyQueue.empty.enqueue(e1, Nil).dequeue)
    println(MyQueue.empty.enqueue(e1, Nil).first)
    println(MyQueue.empty.enqueue(e1, Nil).firstOption)

    println(breadthBT(t))
    println(breadthBT(tt))
    println(breadthBT(Empty))



