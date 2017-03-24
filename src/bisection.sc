
def findMiddle(range: (BigDecimal, BigDecimal)) = {
  (range._1 + range._2)/2.0
}

def findLine(f: BigDecimal => BigDecimal)(range: (BigDecimal, BigDecimal)) = {
  range._1 - ((f(range._1)*(range._2-range._1))/(f(range._2) - f(range._1)))
}

def findRoot(f: BigDecimal => BigDecimal,
             g: ((BigDecimal, BigDecimal)) => BigDecimal,
             error: BigDecimal,
             range: (BigDecimal, BigDecimal)) ={

  def apply(range:(BigDecimal, BigDecimal), step: Int): Option[BigDecimal] ={
    if(f(range._1) * f(range._2) > 0){
      println("Can't apply the method")
      None
    }else {
      println("Step " + step)
      println("Testing between " + range._1 + " and " + range._2)
      println("p is " + g(range))
      println("f(a) = " + f(range._1))
      println("f(b) = " + f(range._2))
      println("f(p) = " + f(g(range)))

      if ((range._2 - range._1).abs < error)
        Some((range._1 + range._2)/2.0)
      else if (f(g(range)) == 0)
        Some(g(range))
      else if (f(g(range)) * f(range._1) < 0)
        apply((range._1, g(range)), step + 1)
      else apply((g(range), range._2), step + 1)
    }
  }
  apply(range,1)
}

def function(x: BigDecimal) = x * x * x + x - 50

println(findRoot(function ,findMiddle, 0.00001, (0,30)))
println(findRoot(function ,findLine(function), 0.00001, (0,30)))

