package example

import java.util.Calendar

object OopsBasics {
  class Person(name: String, age: Int) {
    val Name = name
    val Age = age

    // overloading
    def greet(): Unit =
      println(s"Hi, my name is $Name")

    def greet(name: String): Unit =
      println(s"${this.Name} says, 'Hello $name'")

    // multiple constructors -- really just replace default params
    def this(name: String) = this(name, 0)
    def this() = this("John Doe")
  }

  val person = new Person("Bob", 42)
}

object OopsBasics_Exercises {
  /*
  * Novel and Writer
  * 
  * Writer: first name lastname age
  *  - fullname
  *
  * Novel: name, year of release, author
  *  - author age
  *  - isWrittenBy (author)
  *  - copy (new year of relese) = new instance of Novel
  *
  *
  * Counter Class
  *  - recieves an int value
  *  - method current count
  *  - method to inc/dec => new Counter
  *  - overload inc/dec to receive an amount
  *
  */

  class Writer(
    val firstName: String,
    val lastName: String,
    born: Int
  ) {
    def age = Calendar.getInstance.get(Calendar.YEAR) - born
    def fullName = f"$firstName $lastName"
  }

  class Novel(
    val title: String,
    val release: Int,
    val author: Writer
  ) {
    def authorAge = author.age
    def isWrittenBy = author.fullName
    def copy = (year: Int) => new Novel(title, year, author)
  }

  class Counter(val count: Int) {
    // overloaded
    // def inc() = new Counter(count + 1)
    // def inc(by: Int) = new Counter(count + by)
    // def dec() = new Counter(count - 1)
    // def dec(by: Int) = new Counter(count - by)

    def inc(by: Int = 1): Counter = new Counter(count + by)
    def dec(by: Int = 1): Counter = new Counter(count - by)
  }
}

object MethodNotations {
  class Person(val name: String, favoriteMovie: String) {
    def likes(movie: String): Boolean = movie == favoriteMovie
    def hangoutWith(person: Person): String = s"$name is hangingout with ${person.name}"
    def +(person: Person): String = s"$name just got hitched to ${person.name}"
    def unary_! : String = s"bizarro $name has been created, may god have mercy on your soul..."
    def isAlive: Boolean = true
    def apply(): String = s"Hi, my name is ... what"
  }

  val mary = new Person("Mary", "Inception")
  val matt = new Person("Matt", "Fight Club")

  mary.likes("Inception")

  // sugah -- unary is cool -- rest seems consequential of operator artifacts
  mary likes "Inception"
  mary hangoutWith matt
  mary + matt
  !mary
  mary()
}

object MethodNotations_Excercises {
  /*
  * Overload ! operator
  *
  * Add an age to person class
  * Add a unary + operator => new person with age + 1
  *
  * Add learns method in person class
  * Add learns scala method, no params - calls learns method with "Scala"
  * postfix notation
  * 
  * Overload the apply method
  * mary.apply(2) => "Marry watched Inception n times"
  */
  class Person(val name: String, age: Int, favoriteMovie: String) {
    def apply(): String = s"Hi, my name is ... what"
    def apply(watched: Int): String = s"Mary watched $favoriteMovie $watched time(s)"

    def unary_! : String = s"bizarro $name has been created, may god have mercy on your soul..."

    def +(person: Person): String = s"$name just got hitched to ${person.name}"
    def +(): Person = new Person(name, age + 1, favoriteMovie)
    def +(nickname: String): String = s"$nickname is stupid"

    def learns(): String = learns("Scala")
    def learns(thing: String): String = s"$name is learning $thing"

    def likes(movie: String): Boolean = movie == favoriteMovie
    def hangoutWith(person: Person): String = s"$name is hangingout with ${person.name}"
    def isAlive: Boolean = true
  }

  object Objects {

    object Person {
      val N_EYES = 2
      def canFly: Boolean = false
    }

    object Pirate {
      val N_EYES = 1
      def canFly: Boolean = true
      def apply = new Priate
    }

    class Priate
  }

  object InheritanceAndTraits {
    // constructors
    class Person(name: String, age: Int)
    class Adult(name: String, age: Int, idCard: String)
      extends Person(name, age)

    class Animal {
      val creatureType = "wild"
      def eat = println("nomnom")
    }
    class Cat extends Animal {
      def crunch = {
        eat
        println("crunch crunch")
      }
    }
    val cat = new Cat
    cat.crunch

    // overriding
    class Dog(override val creatureType: String) extends Animal {
      override def eat: Unit = println("crunch crunch")
    }

    val ralph = new Dog("Dump")
    val sammy = new Dog("Pet")
    sammy.eat

    // preventing extension
    // 1 - use final on member
    // 2 - use final on class
    // 3 - seal the class (file specific)
  }

  object AbstractDataTypes {
    abstract class Animal {
      val creatureType: String
      def eat: Unit
    }

    class Dog extends Animal {
      override val creatureType: String = "Pet"
      override def eat: Unit = println("nom nom")
    }

    trait Carnivore {
      val preferredMeal: String = "fresh meat"
      def eat(animal: Animal): String
    }
    trait ColdBlooded

    class Crocodile extends Animal with Carnivore with ColdBlooded {
      val creatureType: String = "croc"
      def eat: Unit = println("I'm a croc, nom nom nom")
      def eat(animal: Animal): String = s"I'm a cros and I'm eathing a ${animal.creatureType}"
    }

    val dog = new Dog
    val croc = new Crocodile
    croc.eat(dog)
    // croc.preferredMeal
  }

  object MyList_Excercise {
    /*
     * head - first element of the list
     * tail - remainder of the list
     * isEmpty - bool
     * add(int) -> new list with element added
     * toString -> a string representation of the list
     */
    abstract class MyList {
      def head: Int
      def tail: MyList
      def isEmpty: Boolean
      def add(element: Int): MyList
      def printElements: String
      override def toString(): String = s"[${printElements}]"
    }

    object Empty extends MyList {
      def head: Int = throw new NoSuchElementException
      def tail: MyList = throw new NoSuchElementException
      def isEmpty: Boolean = true
      def add(element: Int): MyList = new Cons(element, Empty)
      def printElements: String = ""
    }

    class Cons (h: Int, t: MyList) extends MyList {
      def head: Int = h
      def tail: MyList = t
      def isEmpty: Boolean = false
      def add(element: Int): MyList = new Cons(element, this)
      def printElements: String = 
        if(t.isEmpty) "" + h
        else f"${h} ${t.printElements}"
    }

    val list = new Cons(1, new Cons(2, new Cons(3, Empty)))

  }

  object Generics {
    class Dingo[A] {
      // use the type A
    }

    class MyMap[Key, Value]

    val dingoInt = new Dingo[Int]
    val dingoString = new Dingo[String]

    object Dingo {
      def empty[A]: Dingo[A] = ???
    }

    // variance problem
    class Animal
    class Cat extends Animal
    class Dog extends Animal

    // 1. yes, List[Cat] extends List[Animal] = COVARIANCE
    class CovariantList[+A]
    val animal: Animal = new Cat
    val animalList: CovariantList[Animal] = new CovariantList[Cat]
    // animalList.add(new Dog) ??? HARD QUESTION

    // 2. No = INVARIANCE
    class InvariantList[A]
    val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]

    // 3. CONTRAVARIANCE
    class Trainer[-A]
    val trainer: Trainer[Cat] = new Trainer[Animal]
    // logic: "because a cat belongs to animal, an animal trainer can train a cat"

    // bounded types
    class Cage[A <: Animal](animal: A)
    val vage = new Cage(new Dog)
  }

  object Generics_Excercise {
    abstract class MyList[+A] {
      def head: A
      def tail: MyList[A]
      def isEmpty: Boolean
      def add[B >: A](element: B): MyList[B]
      def printElements: String
      override def toString(): String = s"[${printElements}]"
    }

    object Empty extends MyList[Nothing] {
      def head: Nothing = throw new NoSuchElementException
      def tail: Nothing = throw new NoSuchElementException
      def isEmpty: Boolean = true
      def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
      def printElements: String = ""
    }

    class Cons[+A] (h: A, t: MyList[A]) extends MyList[A] {
      def head: A = h
      def tail: MyList[A] = t
      def isEmpty: Boolean = false
      def add[B >: A](element: B): MyList[B] = new Cons(element, this)
      def printElements: String = 
        if(t.isEmpty) "" + h
        else f"${h} ${t.printElements}"
    }
  }

  object AnonymousClasses {
    abstract class Animal {
      def eat: Unit
    }

    val funnyAnimal: Animal = new Animal {
      override def eat: Unit = println("silly")
    }

    abstract class Person {
      val firstName: String
      val lastName: String
    }

    val bob = new Person {
      val firstName = "Bob"
      val lastName = "Ross"
    }
  }

  object More_Exercises {
    /*
     * 1. Generic trait MyPredicate[-T] test(T) => boolean
     * 2. Generic trait MyTransformer[-A, B]
     * 3. MyList:
     *  - map(transformer) -> MyList
     *  - filter(predicate) -> MyList
     *  - flatMap(transformer from A to MyList[B]) => MyList[B]
     *
     *  class EvenPredicate extends MyPredicate[Int]
     *  class StringToIntTransformer extends MyTransformer[String, Int]
     *
     *  [1,2,3].map(double) -> [2, 4, 6]
     *  [1,2,3,4].filter(even) -> [2,4]
     *  [1,2,3].flatMap(n -> [n, n+ 1]) => [1,2,2,3,3,4]
     */

    // crap implementation of filter
    trait MyPredicate[-T] {
      def test(elem: T): Boolean
    }
    // crap implementation of map
    trait MyTransformer[-A, B] {
      def transform(elem: A): B
    }
    abstract class MyList[+A] {
      override def toString(): String = s"[${printElements}]"

      def head: A
      def tail: MyList[A]
      def isEmpty: Boolean
      def printElements: String
      def add[B >: A](element: B): MyList[B]
      def map[B](transformer: MyTransformer[A, B]): MyList[B]
      def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
      def filter(predicate: MyPredicate[A]): MyList[A]
      def ++[B >: A](list: MyList[B]): MyList[B]
    }

    object Empty extends MyList[Nothing] {
      def printElements: String = ""

      def head: Nothing = throw new NoSuchElementException
      def tail: Nothing = throw new NoSuchElementException
      def isEmpty: Boolean = true
      def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
      def map[B](transformer: MyTransformer[Nothing,B]): MyList[B] = Empty
      def flatMap[B](transformer: MyTransformer[Nothing,MyList[B]]): MyList[B] =  Empty
      def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] =  Empty
      def ++[B](list: MyList[B]): MyList[B] = list
    }

    class Cons[+A] (h: A, t: MyList[A]) extends MyList[A] {
      def printElements: String = 
        if(t.isEmpty) "" + h
        else f"${h} ${t.printElements}"

      def head: A = h
      def tail: MyList[A] = t
      def isEmpty: Boolean = false

      def add[B >: A](element: B): MyList[B] =
        new Cons(element, this)

      def map[B](transformer: MyTransformer[A,B]): MyList[B] =
        new Cons(transformer.transform(h), t.map(transformer))

      def flatMap[B](transformer: MyTransformer[A,MyList[B]]): MyList[B] =
        transformer.transform(h) ++ t.flatMap(transformer)

      def filter(predicate: MyPredicate[A]): MyList[A] =
        if (predicate.test(h)) new Cons(h, t.filter(predicate))
        else t.filter(predicate)

      def ++[B >: A](list: MyList[B]): MyList[B] = 
        new Cons(h, t ++ list)
    }

    val list1 = new Cons(1, new Cons(2, new Cons(3, Empty)))
    val list2 = new Cons(4, new Cons(5, Empty))

    def isOdd(num: Int) = num % 2 != 0
    def isEven(num: Int) = num % 2 == 0
    val oddNumbers = new MyPredicate[Int] {
      def test(elem: Int): Boolean = isOdd(elem)
    }
    val evenNumbers = new MyPredicate[Int] {
      def test(elem: Int): Boolean = isEven(elem)
    }

    def double(num: Int) = num + num
    val doubleList = new MyTransformer[Int, Int] {
      def transform(elem: Int): Int = double(elem)
    }
  }

  object CaseClasses {

    case class Person(name: String, age: Int)

    // 1. class parameters are fields
    val joe = new Person("Joe", 40)
    println(joe.age)
    println(joe.name)

    // 2. sensible toString
    println(joe) // => Person("Joe",40)

    // 3. equals and hashcode implemented OOTB
    val joe2 = new Person("Joe", 40)
    println(joe == joe2)

    // 4. case classes have handy copy method
    val joe3 = joe.copy(age = 100)
    println(joe3)

    // 5. have companion objects
    val thePerson = Person
    val mary = Person("Mary", 32)

    // 6. are serializable
    // Akka

    // 7. CCs have extractor patterns
    // can be used in Pattern Matching

    case object UnitedKingdom {
      def name: String = "The UK of GB and NI"
    }

  }
}
