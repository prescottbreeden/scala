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

  def curry(f: (Int, Int) => Int): (Int => Int => Int) =
    x => y => f(x, y)

  def unCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) => f(x)(y)

  def compose[A, B, C](f: A => B, g: C => A): C => B =
    x => f(g(x))

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    x => g(f(x))

  def superAdder2: (Int => Int => Int) = curry(_ + _)
  def add4 = superAdder2(4)
  add4(38)
}

object Map_FlatMap_ForComprehensions {
  val list = List(1, 2, 3)
  list.map(_ * 2)

  val toPair = (x: Int) => List(x, x + 1)
  list.flatMap(toPair)

  val nums = List(1, 2, 3, 4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white")

  // challenge: generate List ("a1-white"..."d4-black")
  nums.flatMap(n =>
    chars.flatMap(c => colors.map(color => s"${c}${n}-${color}"))
  )

  // for comprehensions
  val combinations = for {
    n <- nums
    c <- chars
    color <- colors
  } yield s"${c}${n}-${color}"

  val combinationsWithFilters = for {
    n <- nums if n % 2 == 0
    c <- chars if c == 'a'
    color <- colors if color == "black"
  } yield s"${c}${n}-${color}"

  // challenge: generate List ("a1"..."d4")
  nums.flatMap(n => chars.map(c => s"${c}${n}"))

  //foreach
  list.foreach(println)

  for {
    n <- nums
  } println(n)

  // syntax overload
  list.map { x =>
    x * 2
  }

  list.map { _ * 2 }

  list filter { _ % 2 == 0 }
}

object exercises {
  /*
   *  1. MyList supports for comprehesions?
   *  2. A small collection of at most ONE element - Maybe[+T]
   *    - map, flatMap, filter
   */

  val list1 = Cons(1, Cons(2, Cons(3, Empty)))
  val list2 = Cons(4, Cons(5, Cons(6, Empty)))
  val bob = list1 ++ list2

  val nums = for {
    n <- bob
  } yield s"\nCommencing licks to center of tootsie pop: $n"

  abstract class Maybe[+A] {
    def filter(p: A => Boolean): Maybe[A]
    def map[B](f: A => B): Maybe[B]
    def flatMap[B](f: A => Maybe[B]): Maybe[B]
  }

  case object Nada extends Maybe[Nothing] {
    def filter(p: Nothing => Boolean): Maybe[Nothing] = Nada
    def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Nada
    def map[B](f: Nothing => B): Maybe[B] = Nada
  }

  case class Just[+T](value: T) extends Maybe[T] {
    def filter(p: T => Boolean): Maybe[T] =
      if (p(value) == true) Just(value)
      else Nada

    def map[B](f: T => B): Maybe[B] = Just(f(value))
    def flatMap[B](f: T => Maybe[B]): Maybe[B] = f(value)
  }

  val just3 = Just(3)
  just3.map(_ * 2)

}

object collections_overview {
  /*
   * scala.collections.immutable
   *    - Traversable
   *      - Iterable
   *        - Set: do not contain duplicates
   *          := HashSet
   *          := SortedSet
   *
   *        - Map: association between keys -> values
   *          := HashMap
   *          := SortedMap
   *
   *        - Seq: collections that can be traversed in a set order
   *          := IndexedSeq (quickly accessed)
   *              - Vector
   *              - Range
   *              - String
   *          := LinearSeq
   *              - List
   *              - Stream
   *              - Stack
   *              - Queue
   *
   *
   * scala.collections.MUTABLE
   *    - Traversable
   *      - Iterable
   *        - Set: do not contain duplicates
   *          := HashSet
   *          := LinkedHashSet
   *
   *        - Map: association between keys -> values
   *          := HashMap
   *          := MultiMap
   *
   *        - Seq: collections that can be traversed in a set order
   *          := IndexedSeq
   *              - StringBuilder
   *              - ArrayBuffer (extends indexed and buffer)
   *          := Buffer
   *              - ArrayBuffer (extends indexed and buffer)
   *              - ListBuffer
   *          := LinearSeq
   *              - LinkedList
   *              - MutableList
   *
   */

  // sequences

  val seq = Seq(1, 2, 3, 4)
  val range = 1 to 10
  val list = List(1, 2, 3)
  val prepended1 = 42 :: list
  val prepended2 = 42 +: list
  val weeeeeeeee = 42 +: list :+ 89
  val apples5 = List.fill(5)("apple")
  apples5.mkString("-|-")

  // Arrays

  val numbers = Array(1, 2, 3)
  val threeElements = Array.ofDim[Int](3)
  //mutation
  numbers(2) = 0 // syntax sugar for numbers.update(2, 0)

  // Arrays and Seqs
  val numbersSeq: Seq[Int] = numbers // implicit conversion

  // Vector
  val vector = Vector(1, 2, 3)

  // vectors vs lists

  import scala.util.Random
  val maxRuns = 1000
  val maxCapacity = 1000000
  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTume = System.nanoTime()
      collection.updated(r.nextInt(maxCapacity), 0)
      System.nanoTime() - currentTume
    }
    times.sum * 1.0 / maxRuns
  }

}

object TuplesAndMaps {

  val twople = Tuple2(2, "I'm a two-ple")
  val threeple = Tuple3(2, 3, "I'm a three-ple")
  val easy = (2, "dingo")
  easy.swap
  easy.copy(_2 = "ate my semicolons")
  val map = Map(1 -> "rubber", 2 -> "baby", 3 -> "buggy", 4 -> "bumpers")

  val phoneBook = Map(("jim", 555), ("bob", 123), ("mary", 789))

  val users = Map((1, ("jim", 555)), (2, ("bob", 123)), (3, ("mary", 789)))

  users contains 2

  val pair = 4 -> ("dingo", 1337)
  val updated = users + pair

  users map { user => user._2._1.toLowerCase -> user._2._2 }
  updated filter { _._1 == 4 }

  phoneBook.mapValues(n => n * 10)
  phoneBook.toList
  List(("dingo", 555)).toMap //-> Map(dingo -> 555)

  val names = List("Bob", "Janes", "Angela", "Mary", "Bart", "June")
  names.groupBy(name => name.charAt(0))

  // use lamdas for inlining
  def add: Int => Int => Int = a => b => a + b
  def isEqual: Any => Any => Boolean = a => b => a == b

  List(1, 2, 3, 4) filter { add(42) andThen isEqual(45) }

  def toLowerCase: String => String = s => s.toLowerCase
  def startsWith: String => String => Boolean = s => base => base.startsWith(s)

  Map[String, Int]("Jim" -> 1, "Jack" -> 2, "Mary" -> 3) filterKeys {
    startsWith("j") compose toLowerCase
  }
  Map[String, Int]("Jim" -> 1, "Jack" -> 2, "Mary" -> 3) filterKeys {
    toLowerCase andThen startsWith("j")
  }

}

object tuples_maps_exercises {
  /*
   *  1. What would happen if we map two orig entries "Jim" "JIM" into matching
   *  2. Overly simplified social network based on maps
   *      Person = (String, List(Person))
   *      - add a person to network
   *      - remove a person from network
   *      - friend (mutual)
   *      - unfreind
   *
   *      - number of friends of a person
   *      - person with most friends
   *      - how many people have NO friends
   *      - if there is a social connection between two people (degree of sep)
   */

  val phoneBook = Map(
    ("Jim", 1),
    ("JIM", 2),
    ("bob", 123),
    ("mary", 789)
  )

  phoneBook map { entry => entry._1.toLowerCase -> entry._2 }
// destroys new matching hashes with latest (unless hash exists already)

}

object Optionals {
  val maybe42: Option[Int] = Some(42)
  val maybe28: Option[Int] = None

  def unsafeMethod(): String = null
  val result = Some(unsafeMethod()) // Some(null) non non ono
  val better = Option(unsafeMethod())

  def backupMethod(): String = "A valid result"
  val chainedResult = Option(unsafeMethod()).orElse(Option(backupMethod()))

  // Design beter APIS
  def betterUnsafe(): Option[String] = None
  def betterBackup(): Option[String] = Some("A valid result")
  val betterChained = unsafeMethod() orElse backupMethod()

  maybe42.map(_ * 2)
  maybe42.filter(_ < 10)
  maybe42.flatMap(x => Option(x * 10))
}

object Options_exercises {
  import scala.util.Random
  val config: Map[String, String] = Map("host" -> "176.23.24.1", "port" -> "80")

  class Connection {
    def connect = "Connected" // connect to some server
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  // try to establish a connection, if so - print the connect method
  val host = config.get("host")
  val port = config.get("port")

  val connection = host.flatMap(h => port.flatMap(p => Connection(h, p)))
  val connectionStatus = connection.map(_.connect)

  // chained calls
  config
    .get("host")
    .flatMap(host =>
      config
        .get("port")
        .flatMap(port => Connection(host, port))
        .map(_.connect)
    )

  // for comprehesion
  val status = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } yield connection.connect
}

object handling_failure {
  import scala.util.Failure
  import scala.util.Success
  import scala.util.Try
  // create success and failure explicitly
  val success = Success(3)
  val failure = Failure(new RuntimeException("SUPER FAILURE"))

  def unsafeMethod(): String = throw new RuntimeException(
    "NO STRING FOR YOU BUSTER"
  )
  val potentialFailure = Try(unsafeMethod())

  val anotherPotentialFail = Try {
    // code that might throw
  }

  // utilities
  potentialFailure.isSuccess
  potentialFailure orElse Try(Success("Dingo ate my semicolons"))

}

object excercise {
  import scala.util.Random
  import scala.util.Try

  val hostname = "localhost"
  val port = "8080"
  def renderHTML(page: String) = page

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interupted")
    }

    def getSafe(url: String): Try[String] = Try(get(url))
  }

  object HttpService {
    val random = new Random(System.nanoTime())
    def getConnection(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Port is already in use")
    }

    def getSafeConnection(host: String, port: String): Try[Connection] =
      Try(getConnection(host, port))
  }

  // imperative
  val possibleConnection = HttpService.getSafeConnection(hostname, port)
  val possibleHtml =
    possibleConnection.flatMap(connection => connection.getSafe("/home"))
  possibleHtml.map(renderHTML)

  // dot chained
  HttpService
    .getSafeConnection(hostname, port)
    .flatMap(connection => connection.getSafe("/home"))
    .map(renderHTML)

  // for comprehesion
  for {
    connection <- HttpService.getSafeConnection(hostname, port)
    html <- connection.getSafe("/home")
  } renderHTML(html)

}
