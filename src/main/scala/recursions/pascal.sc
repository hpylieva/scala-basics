object task1{

  /**
    * Calculates the element of row r and column c in Pascal's Triangle
    */

  def pascal(c: Int, r: Int):Int =
  {
    if(c > r) -1
    else if(r==c || c==0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  }

  pascal(1,0)
  pascal(10,2)

  pascal(0,0)

  pascal(0,1)
  pascal(1,1)

  pascal(0,2)
  pascal(1,2)
  pascal(2,2)

  pascal(0,3)
  pascal(1,3)
  pascal(2,3)
  pascal(3,3)


}