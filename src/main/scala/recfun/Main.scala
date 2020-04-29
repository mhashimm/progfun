package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 1 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(temp: List[Char], orig: List[Char]): Boolean = {
      if (orig.isEmpty) temp.isEmpty
      else if (orig.head == '(') loop(orig.head :: temp, orig.tail)
      else if (orig.head == ')')
        if (!temp.isEmpty) loop(temp.tail, orig.tail)
        else loop(orig.head :: temp, Nil)
      else loop(temp, orig.tail)
    }

    loop(Nil, chars.toList)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 0
    else
    coins match {
      case Nil => 0
      case x::xs if money - x > 0 => countChange(money - x , coins) + countChange(money, xs)
      case x::xs if money - x < 0 => countChange(money, xs)
      case x::xs if money - x == 0 => 1 + countChange(money, xs)
    }
  }
}
