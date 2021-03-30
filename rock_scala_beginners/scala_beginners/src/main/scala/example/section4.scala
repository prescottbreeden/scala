package example

import MyConsList._

object Functions {

  trait MyFunction[A, B] {
    def apply(element: A): B
  }

  val double = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  val stringToIntConverter = new Function1[String, Int] {
    override def apply(string: String): Int = string.toInt
  }

  val adder: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = a + b
  }

  def adder2: ((Int, Int) => Int) = (a: Int, b: Int) => a + b
}

object Functions_Excerices {
  /*
   *  1. create a function which takes 2 string and concatenates them
   *  2. transform the MyPredicate and MyTransformer into function types
   *  3. define a function which takes an int and returns another function
   *      which takes an int
   */
  def concat: ((String, String) => String) = (a: String, b: String) => s"$a$b"
  def add: (Int => Int => Int) = (a: Int) => (b: Int) => a + b

  val list1 = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val list2 = new Cons(4, new Cons(5, Empty))

  def isOdd: Int => Boolean = num => num % 2 != 0
  def isEven: Int => Boolean = num => num % 2 == 0
  def double: Int => Int = num => num + num

  list1.filter(isOdd)
  list1.filter(_ % 2 != 0)
  list1.filter(isEven)
  list1.filter(_ % 2 == 0)
  list2.map(double)
  list2.map(_ * 2)

}

object AnonymousFunctions {
  val double: Int => Int = x => x * 2
  val add: (Int, Int) => Int = (a, b) => a + b
  val just3: () => Int = () => 3

  // curly brace style
  val stringToInt = { (str: String) =>
    str.toInt
  }

  // MOAR sugar
  val niceInc: Int => Int = _ + 1
  val niceAdder: (Int, Int) => Int = _ + _
}

object HOFandCurry {
  type Dingo = (Int => Int, Int) => Int => Int
  type Email = String

  val superFunction: (Int, (String, (Int => Boolean)) => Int) => (Int => Int) =
    ???

  def nTimes(f: Int => Int, times: Int, value: Int): Int =
    if (times <= 0) value
    else nTimes(f, times - 1, f(value))

  def nTimes2(f: Int => Int, n: Int): Int => Int =
    if (n <= 0) identity
    else nTimes2(f, n - 1) andThen f

  def nTimes3: Dingo = (f, n) =>
    x =>
      if (n <= 0) x
      else nTimes3(f, n - 1)(f(x))

  def nTimesEither(f: Int => Int, n: Int): Int => Int =
    n match {
      case n if (n <= 0) => identity
      case _             => nTimesEither(f, n - 1) andThen f
    }

  val plusOne: Int => Int = _ + 1
  val plus10 = nTimes2(plusOne, 10)

  def curriedFormat(c: String)(x: Double): String = c.format(x)

  val standardFormat: Double => String = curriedFormat("%4.2f")
  standardFormat(Math.PI)
  val bob: String = curriedFormat("%4.2f")(3.14)

}

object HOFandCurry_Exercises {
  /*
   *  1. Expand MyList with
   *      - foreach function A => Unit
   *        [1,2,3].foreach((x => println(x))
   *
   *      - sort funtion ((A, A) => Int) => MyList
   *        [1,2,3].sort((x, y) => y - x)
   *        res0: MyList[3,2,1]
   *
   *      - zipWith ((A, A) => B) => MyList[B]
   *        [1,2,3].zipWith(([4,5,6], x * y) => [1*4, 2*5, 3*6]
   *        res0: MyList[4,10,18]
   *
   *      - fold(start)(function) => a value
   *        [1,2,3].fold(0)(x + y)
   *        res0: Int = 6
   *
   *  2.  toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
   *      fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
   *
   *  3.  compose(f,g) => x => f(g(x))
   *      andThen(f,g) => x => g(f(x))
   */


  val list1 = Cons(1, Cons(2, Cons(3, Empty)))
  val list2 = Cons(4, Cons(5, Cons(6, Empty)))
  val bob = list1 ++ list2

  def concat: ((String, String) => String) = (a: String, b: String) => s"$a$b"
  def add: (Int => Int => Int) = (a: Int) => (b: Int) => a + b
  def whack: (Int, Int) => Boolean = (a, b) => 
    if (a == 3) true
    else false


  list1.filter(_ % 2 != 0)
  list1.filter(_ % 2 == 0)
  list2.map(_ * 2)

}
