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
	println (6%5)                             //> 1
	println (6%2)                             //> 0
}