object task3{
  /** Returns the number of possible exchanges of given money using the givel set of coins.
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

  countChange(4, List(1,2))
  countChange(4, List(1,2,3))
  countChange(5000, List(1,3,7))

}