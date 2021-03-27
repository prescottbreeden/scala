object IntroModule {

  def main(args: Array[String]) {
    println("hello")
    println(formatAbs(-42))
    println(basicFib(10))
    println(dynamicFib(100))
    println(findFirst(Array("rubber", "baby", "buggy", "bumpers"), eq("buggy")))
    println(findFirst(Array(1, 2, 3, 42, 5), eq(42)))
    println(add2(40))
    println(sub10(40))
    println(sub20(40))
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def curry[A,B,C] = (f: (A, B) => C) => (a: A) => (b: B) => f(a, b)
  def pipe[A,B,C] = (g: B => C, f: A => B) => (a: A) => g(f(a))
  def compose[A,B,C] = (f: B => C, g: A => B) => (a: A) => f(g(a))

  def eq[A] = (a: A) => (b: A) => a == b
  def add = (a: Int, b: Int) => a + b
  def sub = (a: Int, b: Int) => a - b
  def cadd = curry(add)
  def csub = curry(sub)
  def add2 = cadd(2)
  def add12 = cadd(12)
  def sub12 = csub(12)
  def sub18 = csub(18)
  def sub10 = compose(sub12, add2)
  def sub20 = pipe(add2, sub18)

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def basicFib(n: Int) = {
    def go(n: Int): Int = {
      if (n <= 1) n
      else go(n - 1) + go(n - 2)
    }
    go(n)
  }

  def dynamicFib(n: BigInt) = {
    @annotation.tailrec
    def go(n: BigInt, a: BigInt, b: BigInt): BigInt = {
      if (n == 0) a
      else if (n == 1) b
      else go(n - 1, b, a + b)
    }
    go(n, 0, 1)
  }

  def findFirstString(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def go(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else go(n + 1)

    go(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def go(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else go(n + 1)

    go(0)
  }

}
