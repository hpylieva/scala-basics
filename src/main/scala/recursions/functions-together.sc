
object lab1{

  /** Pascal's Triangle
    * Calculates the element of row r and column c in Pascal's Triangle
    */
  def pascal(c: Int, r: Int):Int =
  {
    if(c > r) -1
    else if(r==c || c==0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  }

  /** Balancing Parantheses
    * Returns 'true' iff the parantheses in the input List are balanced;
    * in case the input List is empty returns true
    */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def balanceMaker(nLeft: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        nLeft == 0
      else if (nLeft < 0)
      /* to stop as soon as disbalance found
       * so no need to wait until the whole string in processed */
        false
      else if (chars.head == '(')
        balanceMaker(nLeft + 1, chars.tail)
      else if (chars.head == ')')
        balanceMaker(nLeft - 1, chars.tail)
      else balanceMaker(nLeft, chars.tail)
    }
    balanceMaker(0, chars)
  }

  /** Money exchanger
    * Returns the number of possible exchanges of given money using the givel set of coins.
    * The list of coins on the input should be with distinct coins.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (!coins.isEmpty && money > 0)
      countChange(money - coins.head, coins)+countChange(money, coins.tail)
    else
      0
  }

}
