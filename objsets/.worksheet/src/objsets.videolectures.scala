package objsets
 val a = new NonEmpty(2, Empty, Empty);
 
object videolectures {
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
    	if (elem < x) new NonEmpty(x, Empty, Empty)
    	else if (elem > x) new NonEmpty(x, Empty, Empty)
    	else this
    		
    }
    def union(other: IntSet): IntSet = other
    override def toString():String = "{"+ left+ elem + right +"}"
    
  }
}
