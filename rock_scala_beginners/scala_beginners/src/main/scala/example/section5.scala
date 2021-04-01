package example

import scala.collection.immutable
import MyConsList._

object PatternMatching {
  import scala.util.Random
  val random = new Random
  val x = random.nextInt(10)

  val description = x match {
    case 1 => "neo"
    case 2 => "double your fun"
    case 3 => "well ok then"
    case _ => "dingos ate my semicolons"
  }

  // Destructuring
  case class Person(name: String, age: Int)
  val bob = Person("Bob Ross", 20)

  val greeting = bob match {
    case Person(n, a) if a < 21 => s"Iz too drunks to drive"
    case Person(n, a)           => s"Hi, my name is $n and I am $a years old"
    case _                      => "I don't know who I am"
  }

  sealed class Animal
  case class Dog(breed: String) extends Animal
  case class Parrot(greeting: String) extends Animal

  val dog: Animal = Dog("dingo")
  val parrot: Animal = Parrot("dingo ate your semicolons")

  def whut(animal: Animal) = animal match {
    case Dog(d)    => d
    case Parrot(p) => p
    case _         => "I dunno"
  }

}

object AllThePatterns {
  val x: Any = "bobby"
  val constants = x match {
    case 1              => "a number"
    case "Scala"        => "The Scala"
    case true           => "The Truth"
    case AllThePatterns => "An object"
    case _              =>
  }

  val matchAnything = x match {
    case _ =>
  }

  val matchVariable = x match {
    case something => s"I've found $something"
  }

  val tuple = (1, 2)
  val matchTuple = tuple match {
    case (1, 1) =>
    case (x, 2) => s"I've found $x with a ._2 == 2"
    case (x, y) =>
  }

  val nestedTuule = (1, (2, 3))
  val matchNested = nestedTuule match {
    case (_, (2, v)) =>
  }

  val list: MyList[Int] = Cons(1, Cons(2, Empty))
  val matchList = list match {
    case Empty                              => "empty bruh"
    case Cons(head, Cons(subhead, subtail)) => subtail
    case Cons(head, tail)                   =>
  }

  val standardList = List(1, 2, 3, 42)
  val standardListMatching = standardList match {
    case List(1, _, _, _)    => // extractor -- advanced
    case List(1, _*)         => // list of arbtirary length
    case 1 :: List(_)        => // infix pattern
    case List(1, 2, 3) :+ 42 => // infix pattern
  }

  val unknown: Any = 2
  val thing = unknown match {
    case list: List[Any] => "number list"
    case string: String  => "string"
    case _               =>
  }

  val nambindingMatch = list match {
    case Cons(1, rest @ Cons(2, _)) => // name binding inside nested patterns
    case nonEmptyList @ Cons(
          _,
          _
        ) => // name binding => use the name latere here
  }

  val multiPattern = list match {
    case Empty | Cons(0, _) =>
  }

  val secondElementSpecial = list match {
    case Cons(_, Cons(specialElement, _)) if specialElement % 2 == 0 =>
  }
}

object pattern_match_excercises {
  val numbers = List(1, 2, 3)
  // type erasure in JVM means no soup for you
  val numbersMatch = numbers match {
    case listOfStrings: List[String] => "list of strings"
    case listOfNumbers: List[Int]    => "list of numbers"
    case _                           => ""
  }
}

object patterns_everywhere {
  // big idea #1
  try {
    // code
  } catch {
    case e: RuntimeException       => "runtime"
    case npe: NullPointerException => "npe"
    case _: Throwable              => "ruh roh"
  }

  // big idea #2
  val list = List(1, 2, 3, 4)
  val evens = for {
    x <- list if x % 2 == 0
  } yield 10 * x

  // gnerators are also based on Pattern matching
  val tuples = List((1, 2), (3, 4))
  val filterTuples = for {
    (first, second) <- tuples
  } yield first * second

  // case classes, :: operators, ...

  // big idea #3 (destructuring)
  val tuple = (1, 2, 3)
  val (a, b, c) = tuple
  println(tuple)

  val head :: tail = list
  println(head)

  // big idea #4 (partial function ... sorta not really...)
  val partialFunction = list map {
    case v if v % 2 == 0 => v + " is even"
    case 1               => "neo"
    case _               => "somethingelse"
  }

  val explicitPartial = list map { x =>
    x match {
      case v if v % 2 == 0 => v + " is even"
      case 1               => "neo"
      case _               => "somethingelse"
    }
  }

}
