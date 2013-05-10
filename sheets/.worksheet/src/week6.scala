object week6 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(111); 
	def isPrime(n: Int): Boolean = {
	  var before =  2 until n
	  before forall(i =>  n%i != 0)
	};System.out.println("""isPrime: (n: Int)Boolean""");$skip(14); val res$0 = 
	
	isPrime(1);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(12); val res$1 = 
	isPrime(2);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(12); val res$2 = 
	isPrime(4);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(12); val res$3 = 
	isPrime(7);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(13); val res$4 = 
	isPrime(21);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(157); 
	
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
    var z = for {
       i <- 0 until xs.size
    } yield xs(i) *ys(i);
    z sum
  };System.out.println("""scalarProduct: (xs: List[Double], ys: List[Double])Double""");$skip(49); val res$5 = 
  
  scalarProduct(List (1, 2, 4), List(1, 4,0));System.out.println("""res5: Double = """ + $show(res$5))}
}
