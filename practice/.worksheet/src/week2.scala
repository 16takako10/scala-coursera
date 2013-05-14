object week2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(185); 
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int, a: Int, b: Int)Int""");$skip(159); 

  def sum2(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  };System.out.println("""sum2: (f: Int => Int)(Int, Int) => Int""");$skip(103); 

  def sum3(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum3(f)(a + 1, b)
  };System.out.println("""sum3: (f: Int => Int)(a: Int, b: Int)Int""");$skip(109); 

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(48); 
  def factorial(x: Int) = product(x => x)(1, x);System.out.println("""factorial: (x: Int)Int""")}

}
