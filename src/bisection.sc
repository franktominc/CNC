
def findMiddle(range: (Double, Double)) = {
  (range._1 + range._2)/2.0
}

def findLine(f: Double => Double)(range: (Double, Double)) = {
  range._1 - ((f(range._1)*(range._2-range._1))/(f(range._2) - f(range._1)))
}

def bisection(f: Double => Double, g: ((Double, Double)) => Double, error: Double, range: (Double, Double)) ={

  def doBisection(range:(Double, Double), step: Int): Either[Double,(Double,Double)] ={
    if(f(range._1) * f(range._2) > 0){
      println("Can't apply the method")
      Right(range)
    }else {
      println("Step " + step)
      println("Testing between " + range._1 + " and " + range._2)
      println("f(a) = " + f(range._1))
      println("f(b) = " + f(range._2))
      println("f(p) = " + f(g(range)))
      if (math.abs(range._2 - range._1) < error)
        Right(range)
      else if (f(g(range)) == 0)
        Left(g(range))
      else if (f(g(range)) * f(range._1) < 0)
        doBisection((range._1, g(range)), step + 1)
      else doBisection((g(range), range._2), step + 1)
    }
  }
  doBisection(range,1)
}

def function(x: Double) = math.pow(math.E,x) + x

println(bisection(function ,findMiddle, 0.00001, (-1,0)))

