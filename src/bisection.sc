
def bisection(f: Double => Double, error: Double, range: (Double, Double)) ={

  def findMiddle(range: (Double, Double)) = {
    (range._1 + range._2)/2.0
  }

  def doBisection(range:(Double, Double), step: Int): (Double, Double) ={
    println("Step " + step)
    println("Testing between " + range._1 + " and " + range._2)
    if(math.abs(range._1) + math.abs(range._2) <= error || f(findMiddle(range)) == 0) range
    else if(f(findMiddle(range))*f(range._1) < 0) doBisection((range._1, findMiddle(range)), step+1)
    else doBisection((findMiddle(range), range._2),step+1)
  }
  doBisection(range,1)
}

bisection(x => x * x + 3 * x - 2, 0.000000001, (-4,-3))

