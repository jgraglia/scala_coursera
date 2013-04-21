import common._

object intset {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(55); 

	val a = Empty incl 3;System.out.println("""a  : IntSet = """ + $show(a ));$skip(10); val res$0 = 
	a incl 4;System.out.println("""res0: IntSet = """ + $show(res$0));$skip(17); val res$1 = 
	a incl 4 incl 1;System.out.println("""res1: IntSet = """ + $show(res$1));$skip(25); 
	
	val b = Empty incl 10;System.out.println("""b  : IntSet = """ + $show(b ));$skip(11); val res$2 = 
	b union a;System.out.println("""res2: IntSet = """ + $show(res$2));$skip(46); val res$3 = 
	b union a contains 3;System.out.println("""res3: Boolean = """ + $show(res$3));$skip(22); val res$4 =                       //
	b union a contains 6;System.out.println("""res4: Boolean = """ + $show(res$4));$skip(23); val res$5 = 
	b union a contains 10;System.out.println("""res5: Boolean = """ + $show(res$5));$skip(181); 
	
	def nth(index:Int, list:List[Int]) : Int = {
		if (index <0 || index > list.size)  	throw new IndexOutOfBoundsException(index+ " not found in "+list);
		list.drop(index).head
	};System.out.println("""nth: (index: Int, list: List[Int])Int""");$skip(25); val res$6 = 
  
  nth(3, List(2,3,4));System.out.println("""res6: Int = """ + $show(res$6))}
  
  
}
  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }
  
  object Empty extends IntSet {
    def contains(x: Int): Boolean =  false
    def incl(x: Int) : IntSet =  new NonEmpty(x, Empty, Empty)
    def union(other: IntSet): IntSet = other
    override def toString():String = "."
  }

  class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet {
    def contains(x: Int): Boolean =  {
	    if(elem < x) left contains x
	    else if (elem > x) right contains x
	    else true
    }
    def incl(x: Int) : IntSet =  {
    	if (x < elem) new NonEmpty(elem, left incl x, right )
    	else if (x > elem) new NonEmpty(elem, left, right incl x)
    	else this
    		
    }
    def union(other: IntSet): IntSet = left union right union other incl elem;
    override def toString():String = "{"+ left + elem + right +"}"
  }
  
  
  