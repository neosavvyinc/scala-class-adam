package recfun
import common._
import java.util.ArrayList

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
    if( r <= 0 || c < 0 ) 
    	return 0
	else if( c==0 || c==r)	
    	return 1
	else
		pascal(c-1,r-1) + pascal(c, r-1)  
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def isBalanced( chars: List[Char], stack: String ): Boolean = {
    
      if( chars.isEmpty ) {
        return stack.isEmpty;
      }
      else if ( chars(0).equals('(') ) {
        println("Handling an open paren: " + chars.head, stack);
        isBalanced(chars.tail, chars.head + stack)
      }
      else if ( chars(0).equals(')')) {
        println("Handling an close paren: " + chars.head, stack);
        !stack.isEmpty && isBalanced(chars.tail, stack.tail)        
      }
      else {
        println("Handling non paren: " + chars, stack);
        isBalanced(chars.tail, stack);
      }
      
    }
    
    return isBalanced( chars, "");
    
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def findCount( aMoney: Int, aCoins: List[Int] ): Int = {
      
      if( aMoney == 0 ) {
        1
      }
      else if( aMoney < 0 || aCoins.isEmpty ) {
        0
      }
      else {
        findCount( aMoney - aCoins.head, aCoins) +
        findCount( aMoney, aCoins.tail) 
      }
      
    }
    
    return findCount(money, coins)
    
  }
}
