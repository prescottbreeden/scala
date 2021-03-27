package example

object Youtube {
  def main(args: Array[String]){
    println("\n================================")
    println("rubber baby buggy bumpers")
    println("================================")

    // var i = 0
    // while(i <= 10) {
    //   println(i)
    //   i += 1
    // }

    // do {
    //   println(i)
    //   i += 1
    // } while (i <= 10)

    // val randLetters = "ABCDEFG"
    // for(i <- 0 until randLetters.length) {
    //   println(randLetters(i))
    // }

    // val aList = List(1,2,3,4,5)
    // for (i <- aList) {
    //   println("List items " + i)
    // }

    // for (i <- 1 to 5; j <- 6 to 10){
    //   println("i : " + i)
    //   println("j : " + j)
    // }

    def printPrimes() {
      val primeList = List(1,2,3,5,7,11)
      for(i <- primeList){
        if(i == 11) return
        if(i != 1) {
          println(i)
        }
      }
    }

    printPrimes

    def guessingGame() {
      var numberGuess = 0
      do{
        print("Guess Number ")
        numberGuess = readLine.toInt
      } while(numberGuess != 15)
      println(s"you guess the secret number $numberGuess")
    }

    def sum(num1: Int = 1, num2 : Int = 1) : Int =
      num1 + num2

    println(sum(40, 2))
    println(sum(num2 = 40, num1 = 2))
    println(sum())

    def sayHi() : Unit =
      println("herro...")

    sayHi

    def factorial(num : BigInt) : BigInt =
      if (num <= 1) 1 else num * factorial(num - 1)

    val res = factorial(10)
    println(res)

    val nums = List(1, 2, 3, 4, 5)
    val sorted = nums.sortWith(_>_)
    println(sorted)


    println
  }
}
