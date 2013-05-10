object week6 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(32); 
  val a ="dfgdfg";System.out.println("""a  : String = """ + $show(a ));$skip(20); val res$0 = 
  a map (x=> (x,1));System.out.println("""res0: scala.collection.immutable.IndexedSeq[(Char, Int)] = """ + $show(res$0));$skip(28); 
  val zz = List("ee", "dl");System.out.println("""zz  : List[String] = """ + $show(zz ));$skip(25); val res$1 = 
  zz.flatten mkString "";System.out.println("""res1: String = """ + $show(res$1));$skip(100); 
  
	def isPrime(n: Int): Boolean = {
	  var before =  2 until n
	  before forall(i =>  n%i != 0)
	};System.out.println("""isPrime: (n: Int)Boolean""");$skip(14); val res$2 = 
	
	isPrime(1);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(12); val res$3 = 
	isPrime(2);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(12); val res$4 = 
	isPrime(4);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(12); val res$5 = 
	isPrime(7);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(13); val res$6 = 
	isPrime(21);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(156); 
	
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
    var z = for {
       i <- 0 until xs.size
    } yield xs(i) *ys(i)
    z sum
  };System.out.println("""scalarProduct: (xs: List[Double], ys: List[Double])Double""");$skip(15); 
  
  var n = 4;System.out.println("""n  : Int = """ + $show(n ));$skip(46); val res$7 = 
  scalarProduct(List (1, 2, 4), List(1, 4,0));System.out.println("""res7: Double = """ + $show(res$7));$skip(430); 
  
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
  };System.out.println("""isSafe: (col: Int, queens: List[Int])Boolean""");$skip(30); val res$8 = 
  
  isSafe(0, List(0, 1, 2));System.out.println("""res8: Boolean = """ + $show(res$8));$skip(21); val res$9 = 
  isSafe(0, List(2));System.out.println("""res9: Boolean = """ + $show(res$9))}
}
