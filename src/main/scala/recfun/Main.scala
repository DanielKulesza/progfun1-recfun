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
      if (c == 0 || c == r) 1 else pascal(c-1,r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      
      def balanceHelper(depth: Int, chars: List[Char]) : Boolean = {
        if (depth < 0){ false
        }else if (chars.isEmpty){ depth == 0
        }else if (chars.head == '(') { balanceHelper(depth+1, chars.tail)
        }else if (chars.head == ')') { balanceHelper(depth-1, chars.tail)
        }else balanceHelper(depth, chars.tail)
      }
      balanceHelper(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      def countChangeHelper(moneyLeft: Int, coins: List[Int]): Int = {
        if (moneyLeft < 0 || coins.isEmpty) 0
        else if (moneyLeft == 0) 1
        else countChangeHelper(moneyLeft, coins.tail) + countChangeHelper(moneyLeft - coins.head, coins)
      }
      countChangeHelper(money, coins)
    }

  }
