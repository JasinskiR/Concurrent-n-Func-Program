import scala.annotation.tailrec
// Rafał Jasiński

//  zadanie 1
def take[A](n: Int, xs: List[A]): List[A] =
  xs match
    case h :: t =>
      if n > 0 then
        h :: take(n - 1, t)
      else Nil
    case Nil => Nil

take(2, List(1, 2, 3, 5, 6)) == List(1, 2)
take(-2, List(1, 2, 3, 5, 6)) == Nil
take(8, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
take(2, Nil) == Nil
take(1, List("Ala")) == List("Ala")


// zadanie 2
@tailrec
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match
    case _ :: t =>
      if n > 0 then
        drop(n - 1, t)
      else xs
    case Nil => Nil

drop(2, List(1, 2, 3, 5, 6)) == List(3, 5, 6)
drop(-2, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
drop(8, List(1, 2, 3, 5, 6)) == Nil
drop(1, List("Ala ", "ma ", "kota")) == List("ma ", "kota")
drop(5, Nil) == Nil
drop(1, List("Ala")) == Nil


// zadanie 3
def reverse[A](xs: List[A]): List[A] =
  @tailrec
  def rev(xs: List[A], xs_rev: List[A]): List[A] =
    xs match
      case h :: t => rev(t, h :: xs_rev)
      case Nil => xs_rev

  rev(xs, Nil)

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil) == Nil
reverse(List(5)) == List(5)


// zadanie 4
val replicate: List[Int] => List[Int] = xs =>
  def repli(n: Int, x: Int, t: List[Int]): List[Int] =
    if n > 0 then
      x :: repli(n - 1, x, t)
    else replicate(t)

  xs match
    case h :: t => repli(h, h, t)
    case Nil => Nil

replicate(List(1, 0, 4, -2, 3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(Nil) == Nil


// zadanie 5
val root3: Double => Double = a =>
  val epsilon = 10e-15
  @tailrec
  def rooti(root: Double): Double =
    if Math.abs(Math.pow(root, 3) - a) <= epsilon * Math.abs(a) then
      root
    else
      rooti(root + (a / Math.pow(root, 2) - root) / 3)

  if a > 1 then
    rooti(a / 3)
  else rooti(a)

math.abs(root3(-8.0) + 2.0) <= 1.0e-70
math.abs(root3(-1.0e-6) + 1.0e-2) <= 1.0e-16
math.abs(root3(0.0)) == 0.0
math.abs(root3(8.0e18) - 2.0e6) <= 1.0e-70