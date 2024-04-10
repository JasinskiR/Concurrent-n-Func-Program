// Rafał Jasiński

//  zadanie 1
val suma: List[Double] => Double = xs =>
  if xs == Nil then 0.0
  else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

// zadanie 2
def ends[A](xs: List[A]): (A, A) =
  if xs == Nil then
    throw new NoSuchElementException("empty list")
  else if xs.tail == Nil then
    (xs.head,xs.head)
  else (xs.head,ends(xs.tail)._2)

ends(List(1, 3, 5, 6, 9)) == (1,9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1,1)
ends(Nil) // =>> wyjątek NoSuchElementException: empty list

// zadanie 3
val posortowana: List[Int] => Boolean = xs =>
  if xs == Nil || xs.tail == Nil then
    true
  else (xs.head <= xs.tail.head) && posortowana(xs.tail)

posortowana(List(1,3,3,5,6,7)) == true
posortowana(Nil) == true
posortowana(List(1)) == true
posortowana(List(2,1)) == false

// zadanie 4
val glue: (List[String], String) => String = (xs, sep) =>
  if xs == Nil then
    ""
  else if xs.tail == Nil then
    xs.head
  else xs.head + sep + glue(xs.tail, sep)

glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""
glue(List("To", "jest", "napis"), "") == "Tojestnapis"
glue(List("To", "jest", "napis"), "<>") == "To<>jest<>napis"
glue(List("Element"), "-") == "Element"
