package section2

object ValuesVariablesTypes {
  val x = 42

  val num: Int        = 42
  val str: String     = "hello"
  val bool: Boolean   = true
  val char: Char = 'h'
  val short: Short = 4613
  val long: Long = 20198342342434L
  val float: Float = 2.0f
  val double: Double  = 3.14

}

object Expressions {
  val x = 1 + 2
  val y = 3 == x
}

object Functions {
  def func(a: String, b: Int): String =
    a + " " + b

  def repeatedFunc(string: String, n: Int): String = {
    if(n == 1) string
    else string + repeatedFunc(string, n - 1)
  }

  def sideEffects(string: String, n: Int): Unit = {
    if(n == 1) return
    else {
      println(string)
      sideEffects(string, n - 1)
    }
  }

  def greeting(name: String, age: Int): String =
    f"Hi, my name is ${name} and I am ${age} years old"

  def factorial(n: Int): BigInt = {
    @annotation.tailrec
    def go(n: Int, acc: BigInt): BigInt =
      if (n <= 0) n
      else if (n == 1) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def fibb(n: Int): BigInt = {
    @annotation.tailrec
    def go(n: Int, a: BigInt, b: BigInt): BigInt =
      if (n == 0) a
      else if (n == 1) b
      else go(n - 1, b, a + b)
    go(n, 0, 1)
  }

  def isPrime(n: Int): Boolean = {
    @annotation.tailrec
    def go(test: Int): Boolean =
      if (test == 1) true
      else n % test != 0 && go(test - 1)
    n > 0 && go(n / 2)
  }
}

object CalledBy {
  def calledByValue(x: Long): Unit = {
    println(f"by value ${x}")
    println(f"by value ${x}")
  }

  def calledByName(x: => Long): Unit = {
    println(f"by name ${x}")
    println(f"by name ${x}")
  }

  def infinite(): Int = 1 + infinite()

  def printFirst(x: Int, y: => Int) = println(x)

  // printFirst(infinite(), 42) -- crashes
  // printFirst(42, infinite()) -- does not

}


