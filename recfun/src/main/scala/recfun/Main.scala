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
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) throw new IllegalArgumentException("Must target a cell of the triangle: row:" + r + ",col:" + c);
    def isFirstColumnOfTriangle(): Boolean = c == 0
    def isLastColumnOfTriangle(): Boolean = c == r
    def isEdge(): Boolean = isFirstColumnOfTriangle() || isLastColumnOfTriangle

    if (isEdge()) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def computeValueOf(c: Char): Int = {
      if (c == '(') 1;
      else if (c == ')') -1;
      else 0
    }
    def advance(status: Int, next: List[Char]): Boolean = {
      if (next.isEmpty) status == 0
      else if (status < 0) false
      else advance(status + computeValueOf(next.head), next.tail)
    }
    if (chars.isEmpty) false
    else if (chars.size == 1) false
    else advance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
		  def tryWith(inProgress:Int, coin: Int, remainingCoins: List[Int], sol:List[Int]): Int = {
		    def success(winningSol:List[Int]):Int = {
		      //println("Success for "+money+" with change: "+winningSol)
		      1
		    }
		    def keepSameKind(newAmount:Int, newSol:List[Int]): Int = {
		      tryWith(newAmount, coin, remainingCoins, newSol)
		    }
		    def changeKind(newAmount:Int, newSol:List[Int]): Int = {
		      if(remainingCoins.isEmpty)0
		      else tryWith(newAmount, remainingCoins.head, remainingCoins.tail, newSol)
		    }
		    def takeCoin(): Int = {
		      if (inProgress+coin > money)0
		      else if (inProgress+coin == money) success(sol:::List(coin))
		      else {
		         keepSameKind(inProgress+coin, sol:::List(coin))
		      }
		    }
		    def dontTakeCoinAndChangeKind(): Int = {
		       changeKind(inProgress, sol)
		    }
		    takeCoin()+ dontTakeCoinAndChangeKind()
		    
		  }
		  if (money <= 0) 0
		  else if (coins.isEmpty) 0
		  else {
		    tryWith(0, coins.head, coins.tail, List())
		  }
  }
}