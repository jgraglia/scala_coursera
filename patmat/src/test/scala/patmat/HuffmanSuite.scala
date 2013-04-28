package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(Leaf('b',3)) === 3)
    }
  }
  test("weight of a one level Fork count only leaves under him") {
    new TestTrees {
      assert(weight(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 10)) === 5)
    }
  }
  test("weight of a multilevel tree") {
    new TestTrees {
      assert(weight(Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)) === 9)
    }
  }
  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
 test("chars  of a leaf") {
    new TestTrees {
      assert(chars(Leaf('b',3)) === List('b'))
    }
  }
  test("chars of a one level Fork count only leaves under him") {
    new TestTrees {
      assert(chars(Fork(Leaf('a',2), Leaf('b',3), List('c','d'), 10)) === List('a','b', 'c', 'd'))
    }
  } 
 test("chars ignore dups") {
    new TestTrees {
      assert(chars(Fork(Leaf('a',2), Leaf('b',3), List('b','a'), 10)) === List('a','b'))
    }
  }   
  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("makeCodeTree") {
    new TestTrees {
      val sampleTree = makeCodeTree(
		  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
		  Leaf('t', 2)
	  )
	  assert(weight(sampleTree) === 4)
	  assert(chars(sampleTree) === List('x','e','t'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times : empty") {
     new TestTrees {
	  assert(times( List()) === List())
    }
  }
  test("times : single") {
     new TestTrees {
	  assert(times( List('e')) === List(('e',1)))
    }
  }  
  test("times : one of each") {
    new TestTrees {
	  assert(times( List('x','e','t')) === List(('x', 1), ('e', 1), ('t', 1)))
    }
  }
 test("times : same char again") {
     new TestTrees {
	  assert(times( List('e','e')) === List(('e',2)))
    }
  }    
   test("play with list sort") {
     new TestTrees {
       val l = List(('t', 2), ('e', 1), ('z', 3),('a', 5),('b', 0))
       val sorted = l.sortBy(x=>x._2)
	   assert(sorted === List(('b', 0),('e',1),('t',2),('z',3),('a',5)))
    }
  }   
  test("makeOrderedLeafList empty list") {
    assert(makeOrderedLeafList(List()) === List())
  }
  test("makeOrderedLeafList singleton") {
    assert(makeOrderedLeafList(List(('t', 2))) === List( Leaf('t',2)))
  }
  test("makeOrderedLeafList order") {
    assert(makeOrderedLeafList(List(('e', 23), ('t', 2))) === List( Leaf('t',2), Leaf('e',23)))
    assert(makeOrderedLeafList(List(('a', 23), ('z', 2))) === List( Leaf('z',2), Leaf('a',23)))
    assert(makeOrderedLeafList(List(('z', 23), ('a', 2))) === List( Leaf('a',2), Leaf('z',23)))
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton with empty") {
    assert(singleton(List()) === false)
  }
  test("singleton with multiples") {
      val sampleTree1 = makeCodeTree(
		  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
		  Leaf('t', 2)
	  )
	  val sampleTree2 = makeCodeTree(
		  makeCodeTree(Leaf('y', 1), Leaf('e', 1)),
		  Leaf('t', 2)
	  )
      assert(singleton(sampleTree1 :: sampleTree2 :: Nil) === false)
  }
 test("singleton with single") {
      val sampleTree1 = makeCodeTree(
		  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
		  Leaf('t', 2)
	  )
      assert(singleton(List(sampleTree1)) === true)
  }    
  test("combine empty codetree leave them untouched") {
    val leaflist = List()
    assert(combine(leaflist) === leaflist)
  } 
  test("combine 1 codetree leave them untouched") {
	  val leaflist = List(Leaf('e', 1))
			  assert(combine(leaflist) === leaflist)
  } 
  test("combine 2 codetree leave them untouched") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === leaflist)
  } 
  test("combine of some leaf list keep order") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 5))
    assert(combine(leaflist) === List( Leaf('x',5), Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7)))
  }
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
