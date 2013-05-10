object week6 {
  val a ="dfgdfg"                                 //> a  : String = dfgdfg
  a map (x=> (x,1))                               //> res0: scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((d,1), (f,1
                                                  //| ), (g,1), (d,1), (f,1), (g,1))
  val zz = List("ee", "dl")                       //> zz  : List[String] = List(ee, dl)
  zz.flatten mkString ""                          //> res1: String = eedl
  
	def isPrime(n: Int): Boolean = {
	  var before =  2 until n
	  before forall(i =>  n%i != 0)
	}                                         //> isPrime: (n: Int)Boolean
	
	isPrime(1)                                //> res2: Boolean = true
	isPrime(2)                                //> res3: Boolean = true
	isPrime(4)                                //> res4: Boolean = false
	isPrime(7)                                //> res5: Boolean = true
	isPrime(21)                               //> res6: Boolean = false
	
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
    var z = for {
       i <- 0 until xs.size
    } yield xs(i) *ys(i)
    z sum
  }                                               //> scalarProduct: (xs: List[Double], ys: List[Double])Double
  
  var n = 4                                       //> n  : Int = 4
  scalarProduct(List (1, 2, 4), List(1, 4,0))     //> res7: Double = 9.0
  
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    def leftDiag(): Boolean = {
      val l = for (c <- col to 0 by -1; if queens contains c) yield true
      l contains true
    }
    def rightDiag(): Boolean = {
      val l = for (c <- col to n; if queens contains c) yield true
      l contains true
    }
    if (queens contains col) false
    else if (leftDiag) false
    else if (rightDiag) false
  	else true
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean
  
  isSafe(0, List(0, 1, 2))                        //> res8: Boolean = false
  isSafe(0, List(2))                              //> res9: Boolean = false
}