package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      val aNow = a()
      val bNow = b()
      val cNow = c()
      (bNow*bNow) - 4*(aNow*cNow)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val minB = b() * -1
      val deltaNow = delta()
      val doubleA = 2*a()
      if (deltaNow < 0) Set()
      else
      Set(
        (minB + Math.sqrt(deltaNow)) / doubleA,
        (minB - Math.sqrt(deltaNow)) / doubleA
      )
    }
  }
}
