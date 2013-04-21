import common._

object intset {

	val a = Empty incl 3                      //> a  : IntSet = {.3.}
	a incl 4                                  //> res0: IntSet = {.3{.4.}}
	a incl 4 incl 1                           //> res1: IntSet = {{.1.}3{.4.}}
	
	val b = Empty incl 10                     //> b  : IntSet = {.10.}
	b union a                                 //> res2: IntSet = {.3{.10.}}
	b union a contains 3                      //
                                                  //> res3: Boolean = true
	b union a contains 6                      //> res4: Boolean = false
	b union a contains 10                     //> res5: Boolean = false
	
	def nth(index:Int, list:List[Int]) : Int = {
		if (index <0 || index > list.size)  	throw new IndexOutOfBoundsException(index+ " not found in "+list);
		list.drop(index).head
	}                                         //> nth: (index: Int, list: List[Int])Int
  
  nth(3, List(2,3,4))                             //> java.util.NoSuchElementException: head of empty list
                                                  //| 	at scala.collection.immutable.Nil$.head(List.scala:337)
                                                  //| 	at scala.collection.immutable.Nil$.head(List.scala:334)
                                                  //| 	at intset$$anonfun$main$1.nth$1(intset.scala:17)
                                                  //| 	at intset$$anonfun$main$1.apply$mcV$sp(intset.scala:20)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at intset$.main(intset.scala:3)
                                                  //| 	at intset.main(intset.scala)
  
  
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
  
  
  