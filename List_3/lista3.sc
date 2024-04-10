import scala.annotation.tailrec
// Rafał Jasiński

//zadanie 1

@tailrec
def existsA[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match
    case h :: t => p(h) || existsA(t)(p)
    case Nil => false

existsA(List(5, 1, 2, 3))(_ == 2) == true
existsA(List(1, 3, 4))(_ == 2) == false
existsA(Nil)(_ == 2) == false
existsA(List("a", "b"))(_ == "b") == true

def existsB[A](xs: List[A])(p: A => Boolean): Boolean =
  xs.foldLeft(false)((is_x, x) => p(x) || is_x)

existsB(List(5, 1, 2, 3))(_ == 2) == true
existsB(List(1, 3, 4))(_ == 2) == false
existsB(Nil)(_ == 2) == false
existsB(List("a", "b", "c"))(_ == "b") == true

def existsC[A](xs: List[A])(p: A => Boolean): Boolean =
  xs.foldRight(false)((x, is_x) => p(x) || is_x)

existsC(List(5, 1, 2, 3))(_ == 2) == true
existsC(List(1, 3, 4))(_ == 2) == false
existsC(Nil)(_ == 2) == false
existsB(List("a", "b", "c"))(_ == "b") == true


//zadanie 2
val filterR: [A] => List[A] => (A => Boolean) => List[A] = [A] => (xs: List[A]) => p =>
  xs.foldRight[List[A]](Nil)((x, ys) => if p(x) then x :: ys else ys)

filterR(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > 3) == List(7, 7, 8, 4, 6, 9)
filterR[Int](Nil)(_ > 3) == Nil
filterR(List(1, 2, 3))(_ > 3) == Nil
filterR(List('a', 'b', 'c'))(_ > 'b') == List('c')


//zadanie 3
val remove1a: [A] => List[A] => (A => Boolean) => List[A] = [A] => (xs: List[A]) => p =>
  xs match
    case h :: t =>
      if p(h) then
        t
      else h :: remove1a(t)(p)
    case Nil => Nil

remove1a(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1a(Nil)(_ == 2) == Nil
remove1a(List(1, 3, 4))(_ == 2) == List(1, 3, 4)
remove1a(List(1, 3, 4))(_ == 1) == List(3, 4)
remove1a(List('a', 'b', 'b', 'c'))(_ == 'b') == List('a', 'b', 'c')


val remove1b: [A] => List[A] => (A => Boolean) => List[A] = [A] => (xs: List[A]) => p =>
  @tailrec
  def remove1b_tmp(xs: List[A], result: List[A]): List[A] =
    xs match
      case h :: t =>
        if p(h) then
          print(xs)
          t.reverse_:::(result)
        else remove1b_tmp(t, h :: result)
      case Nil => result.reverse
  remove1b_tmp(xs, Nil)

remove1b(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1b(Nil)(_ == 2) == Nil
remove1b(List(1, 3, 4))(_ == 2) == List(1, 3, 4)
remove1b(List(1, 3, 4))(_ == 1) == List(3, 4)
remove1b(List('a', 'b', 'b', 'c'))(_ == 'b') == List('a', 'b', 'c')


def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) =
  @tailrec
  def split_tmp(xs: List[A], result: List[A], n: Int): (List[A], List[A]) =
    xs match
      case h :: t =>
        if n > 0 then
          split_tmp(t, h :: result, n - 1)
        else (result.reverse, xs)
      case Nil => (result.reverse, Nil)
  split_tmp(xs, Nil, n)

splitAt(List('a', 'b', 'c', 'd', 'e'))(2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt(List('a', 'b', 'c', 'd', 'e'))(6) == (List('a', 'b', 'c', 'd', 'e'), Nil)
splitAt[Any](Nil)(1) == (Nil, Nil)
splitAt(List('a', 'b', 'c', 'd', 'e'))(0) == (Nil, List('a', 'b', 'c', 'd', 'e'))
splitAt(List(1, 2, 3, 4, 5))(5) == (List(1, 2, 3, 4, 5), Nil)


