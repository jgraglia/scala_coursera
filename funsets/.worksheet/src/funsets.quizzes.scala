package funsets

object quizzes {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(223); 
   def sum(f: Int => Int)(a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
           if (a > b) acc
           else loop(a+1,acc+f(a))
        }
        loop(a, 0)
   };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(114); 
   def product(f: Int => Int) (a: Int, b: Int): Int = {
 	   if(a > b) 1
 	   else f(a) * product(f)(a+1, b)
   };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(30); val res$0 = 
   
   product (x=>x*x)(3, 4);System.out.println("""res0: Int = """ + $show(res$0));$skip(45); 

  def fact(a:Int):Int = product(x=>x)(1, a);System.out.println("""fact: (a: Int)Int""");$skip(13); val res$1 = 
  
  fact(3);System.out.println("""res1: Int = """ + $show(res$1));$skip(177); 
  
  def general(noModif:Int)(associate:(Int,Int) => Int)(f: Int => Int) (a: Int, b: Int): Int = {
     if(a > b) noModif:Int
 	   else associate(f(a) , product(f)(a+1, b))
  };System.out.println("""general: (noModif: Int)(associate: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int)Int""");$skip(90); 
  
  def p(f: Int => Int) (a: Int, b: Int): Int = {
 	 general(1)((x,y)=>x*y)(f)(a,b)
  };System.out.println("""p: (f: Int => Int)(a: Int, b: Int)Int""");$skip(87); 
  def s(f: Int => Int) (a: Int, b: Int): Int = {
  	general(0)((x,y)=>x+y)(f)(a,b)
  };System.out.println("""s: (f: Int => Int)(a: Int, b: Int)Int""");$skip(23); val res$2 = 
  product (x=>x)(3, 4);System.out.println("""res2: Int = """ + $show(res$2));$skip(16); val res$3 = 
	p (x=>x)(3, 4);System.out.println("""res3: Int = """ + $show(res$3));$skip(22); val res$4 = 
  
  sum (x=>x)(3, 4);System.out.println("""res4: Int = """ + $show(res$4));$skip(17); val res$5 = 
  s (x=>x)(3, 4)
  
  import math.abs;System.out.println("""res5: Int = """ + $show(res$5));$skip(380); 
  
  def fixedPoint(f:Double=>Double)(firstGuess:Int) : Double = {
    val tolerance = 0.00001
    def isCloseEnough(x:Double, y:Double):Boolean = {
  		abs((x-y)/x) < tolerance
  	}
  	def iterate(guess:Double): Double = {
  		val next =f(guess)
  		println(next)
  		if(isCloseEnough(guess, next))next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  };System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Int)Double""");$skip(63); 
  def averageDamp(f:Double => Double)(x:Double) = (x + f(x))/2;System.out.println("""averageDamp: (f: Double => Double)(x: Double)Double""");$skip(69); 
  
  def sqrt(x:Double)= {
  	fixedPoint(averageDamp(y=>x/y))(1)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(10); val res$6 = 
  sqrt(9)
  //sqrt(4)
  
  class Rational(x:Int, y:Int)  {
  	var numerator = x;
  	var denom = y;
  	def add(that:Rational) = {
  		new Rational(
  			numerator*that.denom +denom*that.numerator,
  			denom * that.denom)
  	}
  	def minus(that:Rational) = add(that.neg)
  	def neg =  new Rational(-numerator, denom)
  	
  	def gcd(a:Int, b:Int):Int =if(b==0)a else gcd(b , a%b)
  	override def toString() = {  	val g = gcd(numerator, denom)
  	 numerator/g+"/"+denom/g;}
  };System.out.println("""res6: Double = """ + $show(res$6));$skip(512); val res$7 = 
  
  new Rational(1, 2).add(new Rational(3,4));System.out.println("""res7: funsets.quizzes.Rational = """ + $show(res$7));$skip(25); val res$8 = 
  new Rational(2, 1).neg;System.out.println("""res8: funsets.quizzes.Rational = """ + $show(res$8));$skip(35); 
  
  
  val x = new Rational(1, 3);System.out.println("""x  : funsets.quizzes.Rational = """ + $show(x ));$skip(29); 
  val y = new Rational(5, 7);System.out.println("""y  : funsets.quizzes.Rational = """ + $show(y ));$skip(29); 
  val z = new Rational(3, 2);System.out.println("""z  : funsets.quizzes.Rational = """ + $show(z ));$skip(22); val res$9 = 
  x.minus(y).minus(z);System.out.println("""res9: funsets.quizzes.Rational = """ + $show(res$9));$skip(12); val res$10 = 
   y.add(y);System.out.println("""res10: funsets.quizzes.Rational = """ + $show(res$10))}
}
