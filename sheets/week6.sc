object week6 {
	def isPrime(n: Int): Boolean = {
	  var before =  2 until n
	  before forall(i =>  n%i != 0)
	}                                         //> isPrime: (n: Int)Boolean
	
	isPrime(1)                                //> res0: Boolean = true
	isPrime(2)                                //> res1: Boolean = true
	isPrime(4)                                //> res2: Boolean = false
	isPrime(7)                                //> res3: Boolean = true
	isPrime(21)                               //> res4: Boolean = false
	
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
    var z = for {
       i <- 0 until xs.size
    } yield xs(i) *ys(i);
    z sum
  }                                               //> scalarProduct: (xs: List[Double], ys: List[Double])Double
  
  scalarProduct(List (1, 2, 4), List(1, 4,0))     //> res5: Double = 9.0
}