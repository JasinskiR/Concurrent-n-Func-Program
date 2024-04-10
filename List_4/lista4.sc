// Rafał Jasiński

//zadanie 1
class MyPair[A, B](var fst: A, var snd: B):
  override def toString: String = s"($fst,$snd)"

val pair1 = new MyPair(1, 2)
pair1.toString == "(1,2)"
val pair2 = new MyPair("Ala", "ma kota")
pair2.toString == "(Ala,ma kota)"
val pair3 = new MyPair(1, "apple")
pair3.toString == "(1,apple)"
val pair4 = new MyPair(List("a"), 1)
pair4.toString == "(List(a),1)"


//zadanie 2
//a
class BankAccount(initialBalance : Double):
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
  override def toString = "%.2f".format(balance)

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance):
  override def deposit(amount: Double): Double = super.deposit(amount - 1)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)

val check_acc = new CheckingAccount(1000)
check_acc.checkBalance == 1000
check_acc.deposit(5) == 1004
check_acc.withdraw(3) == 1000

//b
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance):
  private val free_acc_operation = 3
  private var operation_acc = 0
  override def deposit(amount: Double): Double =
    operation_acc += 1
    super.deposit(if operation_acc <= 3 then amount else amount - 1)
  override def withdraw(amount: Double): Double =
    operation_acc += 1
    super.withdraw(if operation_acc <= 3 then amount else amount + 1)
  def earnMonthlyInterest(): Double =
    if checkBalance < 0 then
      super.withdraw(0.01 * Math.abs(checkBalance))
    else super.deposit(0.01 * Math.abs(checkBalance))

val save_acc1 = SavingsAccount(1000)
save_acc1.earnMonthlyInterest() == 1010.0
save_acc1.checkBalance == 1010
save_acc1.deposit(10) == 1020
save_acc1.deposit(10) == 1030
save_acc1.deposit(10) == 1040
save_acc1.deposit(10) == 1049
save_acc1.withdraw(8) == 1040
val save_acc2 = SavingsAccount(-1000)
save_acc2.earnMonthlyInterest() == -1010.0
save_acc2.checkBalance == -1010
save_acc2.deposit(20) == -990
save_acc2.withdraw(10) == -1000



//zadanie 3
// a
abstract class Zwierz(val imieZ: String):
  def imie(): String = imieZ
  def rodzaj(): String
  def dajGlos(): String
  override def toString: String = s"${rodzaj()} ${imie()} daje głos ${dajGlos()}"

class Pies(imieZ: String = "") extends Zwierz(imieZ):
  override def rodzaj(): String = "Pies"
  override def dajGlos(): String = "Hau, hau!"

val pies1 = Pies()
pies1.toString == "Pies  daje głos Hau, hau!"
val pies2 = Pies("Kruczek")
pies2.toString == "Pies Kruczek daje głos Hau, hau!"

// b
class Kot(imieZ: String = "") extends Zwierz(imieZ) {
  override def rodzaj(): String = "Kot"
  override def dajGlos(): String = "Miau, miau!"
}

class Krowa(imieZ: String = "") extends Zwierz(imieZ) {
  override def rodzaj(): String = "Krowa"
  override def dajGlos(): String = "Muuuuuu!"
}

val kot1 = Kot()
kot1.toString == "Kot  daje głos Miau, miau!"
val kot2 = Kot("Mruczek")
kot2.toString == "Kot Mruczek daje głos Miau, miau!"

val cow1 = Krowa()
cow1.toString == "Krowa  daje głos Muuuuuu!"
val cow2 = Krowa("Mućka")
cow2.toString == "Krowa Mućka daje głos Muuuuuu!"

// c
object TestZwierza:
  def main(args: Array[String]): Unit =
    val zwierzeta: Vector[Zwierz] = Vector(
      new Pies("Kruczek"),
      new Kot("Mruczek"),
      new Kot(),
      new Krowa("Mućka"),
      new Krowa(),
    )
    for (zwierz <- zwierzeta) println(zwierz.toString())

TestZwierza.main(Array())