package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if(c==0 || c==r) 1
    else  pascal(c-1,r-1)+pascal(c,r-1)
  
  
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    /*  
    if(chars.isEmpty) true else {
      
      var nb:Int = 0
      var error:Boolean = false
      
      for(elm <- chars if elm=='(' || elm==')') {
        nb = if(elm=='(') nb+1 else nb-1
        if(nb<0) error=true
      }
       
      if(nb==0 && !error) true else false
        
    }
    */
    
    def balanceIter(countBalancing:Int, list : List[Char]):Boolean = {
      if(countBalancing<0 || list.isEmpty) countBalancing == 0
      else { 
        val c = list.head
        if(c=='(') balanceIter(countBalancing+1, list.tail)
        else if(c==')') balanceIter(countBalancing-1, list.tail)
        else  balanceIter(countBalancing, list.tail)
      }
    }
    
    balanceIter(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0)
      if (money == 0) 1
      else 0
    else
      if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
