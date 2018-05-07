object task2{

  /** Returns 'true' iff the parantheses in the input List are balance;
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

  balance("this (is) demo (of work)".toList)
  balance("=)".toList)
  balance("(fdfs))".toList)
  balance("((((()".toList)
  balance("())()".toList)
}