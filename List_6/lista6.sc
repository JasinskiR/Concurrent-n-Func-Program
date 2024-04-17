import scala.annotation.tailrec
// Rafał Jasiński

//zadanie 1

var count = 0
while (count < 5) {
  println(count)
  count += 1
}

@tailrec
def whileLoop(condition: => Boolean)(expr: => Unit): Unit =
  if condition then
    expr
    whileLoop(condition)(expr)

var count = 0
whileLoop(count < 5) {
  println(count)
  count += 1
}

//zadanie 2
def lrepeat[A](k: Int)(xsl: LazyList[A]): LazyList[A] =
  def lrepeat_internal(x: A)(i: Int)(tail: LazyList[A]): LazyList[A] =
    if i > 0 then
      x #:: lrepeat_internal(x)(i - 1)(tail)
    else lrepeat(k)(tail)
  xsl match
    case p #:: t =>
      if k <= 0 then
        LazyList()
      else lrepeat_internal(p)(k)(t)
    case LazyList() => LazyList()

lrepeat(3)(LazyList('a', 'b', 'c', 'd')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()
lrepeat(-1)(LazyList.from(1)).take(2).toList == List()

//zadanie 3
enum BT[+A]:
  case Empty
  case Node(elem: A, left: BT[A], right: BT[A])

import BT.*

val tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

enum lBT[+A]:
  case LEmpty
  case LNode(elem: A, left: () => lBT[A], right: () => lBT[A])

import lBT.*

// a
def lBreadth[A](ltree: lBT[A]): LazyList[A] =
  def lBreadth_internal(queue: List[lBT[A]]): LazyList[A] =
    queue match
      case LNode(elem, left, right) :: t => elem #:: lBreadth_internal(t ::: List(left(), right()))
      case LEmpty :: t => lBreadth_internal(t)
      case Nil => LazyList()

  lBreadth_internal(List(ltree))

lBreadth(LEmpty) == LazyList()

// b
def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
  bt match
    case Empty => acc
    case Node(elem, left, right) => f(elem)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))

def BT2lBT[A](bt: BT[A]): lBT[A] =
  foldBT[A, lBT[A]]((value: A) => (left: lBT[A], right: lBT[A]) => LNode(value, () => left, () => right))(LEmpty)(bt)

lBreadth(BT2lBT(tt)).force == LazyList(1, 2, 3, 4, 5, 6)
lBreadth(LEmpty) == LazyList()
val text_node = Node("Ala::", Node("ma::", Empty, Empty), Node("kota::", Empty, Empty))
lBreadth(BT2lBT(text_node)).force == LazyList("Ala::", "ma::", "kota::")


// c
def lTree(n: Int): lBT[Int] =
  if n <= 0 then
    LEmpty
  else LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))

lBreadth(lTree(1)).take(20).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
lBreadth(LEmpty).take(20).toList == List()
lBreadth(lTree(2)).take(10).toList == List(2, 4, 5, 8, 9, 10, 11, 16, 17, 18)