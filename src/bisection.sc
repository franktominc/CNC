
def bisection(f: Double => Double, error: Double, range: (Double, Double)) ={

  def findMiddle(range: (Double, Double)) = {
    (range._1 + range._2)/2.0
  }

  def doBisection(range:(Double, Double), step: Int): Either[Double,(Double,Double)] ={
    if(f(range._1) * f(range._2) > 0){
      println("Can't apply the method")
      Right(range)
    }else {
      println("Step " + step)
      println("Testing between " + range._1 + " and " + range._2)
      if (math.abs(range._1) + math.abs(range._2) <= error)
        Right(range)
      else if (f(findMiddle(range)) == 0)
        Left(findMiddle(range))
      else if (f(findMiddle(range)) * f(range._1) < 0)
        doBisection((range._1, findMiddle(range)), step + 1)
      else doBisection((findMiddle(range), range._2), step + 1)
    }
  }
  doBisection(range,1)
}

bisection(x => x * x + 3 * x - 2, 0.000000001, (0,30))

