package funsets

object quizzes {
   def sum(f: Int => Int)(a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
           if (a > b) acc
           else loop(a+1,acc+f(a))
        }
        loop(a, 0)
   }                                              //> sum: (f: Int => Int)(a: Int, b: Int)Int
   def product(f: Int => Int) (a: Int, b: Int): Int = {
 	   if(a > b) 1
 	   else f(a) * product(f)(a+1, b)
   }                                              //> product: (f: Int => Int)(a: Int, b: Int)Int
   
   product (x=>x*x)(3, 4)                         //> res0: Int = 144

  def fact(a:Int):Int = product(x=>x)(1, a)       //> fact: (a: Int)Int
  
  fact(3)                                         //> res1: Int = 6
  
  def general(noModif:Int)(associate:(Int,Int) => Int)(f: Int => Int) (a: Int, b: Int): Int = {
     if(a > b) noModif:Int
 	   else associate(f(a) , product(f)(a+1, b))
  }                                               //> general: (noModif: Int)(associate: (Int, Int) => Int)(f: Int => Int)(a: Int,
                                                  //|  b: Int)Int
  
  def p(f: Int => Int) (a: Int, b: Int): Int = {
 	 general(1)((x,y)=>x*y)(f)(a,b)
  }                                               //> p: (f: Int => Int)(a: Int, b: Int)Int
  def s(f: Int => Int) (a: Int, b: Int): Int = {
  	general(0)((x,y)=>x+y)(f)(a,b)
  }                                               //> s: (f: Int => Int)(a: Int, b: Int)Int
  product (x=>x)(3, 4)                            //> res2: Int = 12
	p (x=>x)(3, 4)                            //> res3: Int = 12
  
  sum (x=>x)(3, 4)                                //> res4: Int = 7
  s (x=>x)(3, 4)                                  //> res5: Int = 7
  
  import math.abs
  
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
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Int)Double
  def averageDamp(f:Double => Double)(x:Double) = (x + f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
  def sqrt(x:Double)= {
  	fixedPoint(averageDamp(y=>x/y))(1)
  }                                               //> sqrt: (x: Double)Double
  sqrt(9)                                         //> 5.0
                                                  //| 3.4
                                                  //| 3.023529411764706
                                                  //| 3.00009155413138
                                                  //| 3.000000001396984
                                                  //| 3.0
                                                  //| res6: Double = 3.0
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
  }
  
  new Rational(1, 2).add(new Rational(3,4))       //> res7: funsets.quizzes.Rational = 5/4
  new Rational(2, 1).neg                          //> res8: funsets.quizzes.Rational = -2/1
  
  
  val x = new Rational(1, 3)                      //> x  : funsets.quizzes.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : funsets.quizzes.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : funsets.quizzes.Rational = 3/2
  x.minus(y).minus(z)                             //> res9: funsets.quizzes.Rational = -79/42
   y.add(y)                                       //> res10: funsets.quizzes.Rational = 10/7
}