object week1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(378); 
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
  };System.out.println("""sqrt: (x: Double)Double""");$skip(13); val res$0 = 
  sqrt(1e80);System.out.println("""res0: Double = """ + $show(res$0))}
}
