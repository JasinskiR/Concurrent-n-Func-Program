// Rafał Jasiński
enum BT[+A]:
  case Empty
  case Node(elem: A, left: BT[A], right: BT[A])

import BT.*

import scala.annotation.tailrec

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))
val empty = Empty
val single_node = Node(1, Empty, Empty)
val left_node = Node(1, Node(2, Empty, Empty), Empty)
val right_node = Node(1, Empty, Node(3, Empty, Empty))


//zadanie 1
val sumBT: BT[Int] => Int =
  case Empty => 0
  case Node(elem, left, right) => elem + sumBT(left) + sumBT(right)

sumBT(t) == 6
sumBT(tt) == 21
sumBT(empty) == 0
sumBT(single_node) == 1
sumBT(left_node) == 3
sumBT(right_node) == 4

//zadanie 2
def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
  bt match
    case Empty => acc
    case Node(elem, left, right) => f(elem)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))

//zadanie 3
// a
val sumBTfold: BT[Int] => Int = bt =>
  foldBT((value: Int) => (left: Int, right: Int) => value + left + right)(0)(bt)

sumBTfold(t) == 6
sumBTfold(tt) == 21
sumBTfold(empty) == 0
sumBTfold(single_node) == 1
sumBTfold(left_node) == 3
sumBTfold(right_node) == 4

// b
def inorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((value: A) => (left: List[A], right: List[A]) => left ::: value :: right)(Nil)(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(tt) == List(4, 2, 1, 5, 6, 3)
inorderBTfold(empty) == List()
inorderBTfold(single_node) == List(1)
inorderBTfold(left_node) == List(2, 1)
inorderBTfold(right_node) == List(1, 3)

def preorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((value: A) => (left: List[A], right: List[A]) => value :: left ::: right)(Nil)(bt)

preorderBTfold(t) == List(1, 2, 3)
preorderBTfold(tt) == List(1, 2, 4, 3, 5, 6)
preorderBTfold(empty) == List()
preorderBTfold(single_node) == List(1)
preorderBTfold(left_node) == List(1, 2)
preorderBTfold(right_node) == List(1, 3)

//def postorderBTfold[A](bt: BT[A]): List[A] =
//  foldBT((value: A) => (left: List[A], right: List[A]) => (value :: left.reverse ::: right.reverse).reverse)(Nil)(bt)
def postorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((value: A) => (left: List[A], right: List[A]) => right ::: left ::: List(value))(Nil)(bt)

val mix_node = Node("Ala", Node("ma", Empty, Empty), Node("kota", Empty, Empty))
postorderBTfold(t) == List(3, 2, 1)
postorderBTfold(tt) == List(6, 5, 3, 4, 2, 1)
postorderBTfold(empty) == List()
postorderBTfold(single_node) == List(1)
postorderBTfold(left_node) == List(2, 1)
postorderBTfold(right_node) == List(3, 1)
postorderBTfold(mix_node) == List("kota", "ma", "Ala")

//zadanie 4
def mapBT[A, B](f: A => B)(bt: BT[A]): BT[B] =
  foldBT[A, BT[B]]((value: A) => (left: BT[B], right: BT[B]) => Node(f(value), left, right))(Empty)(bt)

mapBT[Int, Int](v => 2 * v)(t) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)
mapBT[Int, Int](v => 2 * v)(tt) == Node(2, Node(4, Node(8, Empty, Empty), Empty), Node(6, Node(10, Empty, Node(12, Empty, Empty)), Empty))
mapBT[Int, Int](v => 2 * v)(empty) == Empty
mapBT[Int, Int](v => 2 * v)(single_node) == Node(2, Empty, Empty)
mapBT[Int, Int](v => 2 * v)(left_node) == Node(2, Node(4, Empty, Empty), Empty)
mapBT[Int, Int](v => 2 * v)(right_node) == Node(2, Empty, Node(6, Empty, Empty))
mapBT[String, String](v => v + "::")(mix_node) == Node("Ala::", Node("ma::", Empty, Empty), Node("kota::", Empty, Empty))


//zadanie 5
case class Graph[A](succ: A => List[A])

val g = Graph((i: Int) =>
  i match
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0, 2)
    case 5 => List(3)
    case n => throw new NoSuchElementException(s"Graph g: node $n doesn't exist")
)

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean =
  @tailrec
  def search(visited: List[A])(to_visit: List[A]): Boolean =
    to_visit match
      case h :: t =>
        if h == to then
          true
        // else search(if visited.contains(h) then visited else h::visited)(if visited.contains(h) then t else t:::g.succ(h))
        else if visited.contains(h) then
          search(visited)(t)
        else search(h :: visited)(t ::: g.succ(h))
      case Nil => false
  search(Nil)(g.succ(from))

pathExists(g)(4, 1)
!pathExists(g)(0, 4)
!pathExists(g)(3, 0)
pathExists(g)(2, 2)
!pathExists(g)(0, 0)