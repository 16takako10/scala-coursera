object week1 {
  def sqrt(x: Double): Double = {
    def sqrtI(guess: Double): Double =
      if (good(guess)) guess
      else sqrtI(improve(guess))

    def good(guess: Double): Boolean =
      abs((guess * guess - x) / x) < 0.0001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    def abs(x: Double): Double = if (x < 0) -x else x

    sqrtI(1)
  }                                               //> sqrt: (x: Double)Double
  sqrt(1e80)                                      //> res0: Double = 1.0000000542691464E40
}