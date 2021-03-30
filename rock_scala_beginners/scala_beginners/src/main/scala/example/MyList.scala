package MyConsList

abstract class MyList[+A] {
  def ++[B >: A](list: MyList[B]): MyList[B]
  def add[B >: A](element: B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def fold[B](start: B)(operator: (B, A) => B): MyList[B]
  def foreach(f: A => Unit): Unit
  def head: A
  def isEmpty: Boolean
  def map[B](transformer: A => B): MyList[B]
  def printElements: String
  def sort(test: (A, A) => Boolean): MyList[A]
  def tail: MyList[A]
  override def toString(): String = s"[${printElements}]"
  def zipWith[B, C](list: MyList[B], zip:(A, B) => C): MyList[C]
}

case object Empty extends MyList[Nothing] {
  def ++[B](list: MyList[B]): MyList[B] = list
  def add[B >: Nothing](element: B): MyList[B] = Cons(element, Empty)
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def fold[B](start: B)(operator: (B, Nothing) => B): MyList[B] = Cons(start, Empty)
  def foreach(f: Nothing => Unit): Unit = Empty
  def head: Nothing = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def printElements: String = ""
  def sort(test: (Nothing, Nothing) => Boolean): MyList[Nothing] = Empty
  def tail: Nothing = throw new NoSuchElementException
  def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] = 
    if (!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else Empty
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  def ++[B >: A](list: MyList[B]): MyList[B] =
    Cons(h, t ++ list)

  def add[B >: A](element: B): MyList[B] =
    Cons(element, this)

  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def fold[B](start: B)(operator: (B, A) => B): MyList[B] =
    ???

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def head: A =
    h

  def isEmpty: Boolean =
    false

  def map[B](transformer: A => B): MyList[B] =
    Cons(transformer(h), t.map(transformer))

  def printElements: String =
    if (t.isEmpty) "" + h
    else f"${h} ${t.printElements}"

  def sort(compare: (A, A) => Boolean): MyList[A] = 
    tail.filter(x => compare(h, x) == true).sort(compare) ++
      tail.filter(x => compare(h, x) == false).sort(compare).add(h)

  def tail: MyList[A] = t

  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] = 
    if (list.isEmpty) throw new RuntimeException("Lists do not have same length")
    else Cons(zip(h, list.head), t.zipWith(list.tail, zip))
}
