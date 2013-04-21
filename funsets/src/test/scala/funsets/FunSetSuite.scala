package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import FunSets.toString

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val always: Int => Boolean = x => true
    val never: Int => Boolean = x => false
    val oddOnly: Int => Boolean = x => x % 2 != 0
    // even= pair
    val evenOnly: Int => Boolean = x => ! oddOnly(x)
    
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def emptySet : Set = x => false
    def set1and2 : Set = x => (x == 1 || x == 2)
    def set2and3 : Set = x => (x == 2 || x == 3)
    def set2and4 : Set = x => (x == 2 || x == 4)

  }
  test("my specific sets") {
    new TestSets {
      assert(contains(set1and2, 1))
      assert(contains(set1and2, 2))
      assert(!contains(set1and2, 3))

      assert(!contains(set2and3, 1))
      assert(contains(set2and3, 2))
      assert(contains(set2and3, 3))

    }
  }
  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")

    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersert contains only elements found in both") {
    new TestSets {

      assert(!contains(intersect(s1, s2), 1), "interset 1")
      assert(!contains(intersect(s1, s2), 2), "interset 2")
      
      assert(contains(intersect(s1, set1and2), 1), "interset 1 with 1,2")
      assert(contains(intersect(set1and2, s1), 1), "interset 1,2 with 1")
      
      assert(contains(intersect(s2, set1and2), 2), "interset 2 with 1,2")
      assert(contains(intersect(set1and2, s2), 2), "interset 1,2 with 2")

    }
  }
  
  test("diff contains only elements found in first set and not in second set") {
    new TestSets {

      val diffSet1_2 = diff(s1, s2)
      assert(contains(diffSet1_2, 1), "diff empty - 1")
      assert(!contains(diffSet1_2, 2), "diff empty - 2")
      
      val diffSet2_1 = diff(s2, s1)
      assert(contains(diffSet2_1, 2))
      assert(!contains(diffSet2_1, 1))

      

    }
  }
  
  test("filter contains only elements filtered") {
    new TestSets {
      assert(contains(filter(s1, always), 1))
      assert(!contains(filter(s1, never), 1))
      
      assert(contains(filter(set1and2, oddOnly), 1))
      assert(!contains(filter(set1and2, oddOnly), 2))
      
      assert(!contains(filter(set2and4, oddOnly), 2))
      assert(!contains(filter(set2and4, oddOnly), 4))
      
      def t : Set = x => (x == 1 || x == 3|| x == 4|| x == 5|| x == 7|| x == 1000)
        val f: Int => Boolean = x => x < 5
        assert(!contains(filter(t, f), 5))
        assert(!contains(filter(t, f), 7))
        assert(!contains(filter(t, f), 1000))
        assert(contains(filter(t, f), 1))
        assert(contains(filter(t, f), 3))
        assert(contains(filter(t, f), 4))
        assert(!contains(filter(t, f), 0))
    }
  }
  
  test("forall ") {
    new TestSets {
      assert(forall(s1, always), "if accept all then ok")
      assert(forall(set2and4, always), "if accept all then ok, 2")

      assert(!forall(s1, never), "if deny all then false")

      assert(forall(set2and4, evenOnly), "que des nb pairs")
      assert(!forall(set2and4, oddOnly), "pas d'impairs")
      
      assert(forall(emptySet, always), "with empty, always true")
      assert(forall(emptySet, never), "with empty, always true,2")
      
      assert(!forall(set2and3, evenOnly), "pas que des impairs")
    }
  }

  test("exists ") {
    new TestSets {
      assert(exists(s1, always), "if accept all then ok")
      assert(exists(set2and4, always), "if accept all then ok, 2")
      assert(!exists(intersect(s1, s2), always), "if not element then false")

      assert(!exists(s1, never), "if deny all then false")
      assert(!exists(set2and4, never), "if deny all then false ,2")
      
      assert(exists(set2and3, evenOnly), "un pair doit être détecté dans "+ FunSets.toString(set2and3))
      assert(exists(set2and3, oddOnly), "un impair doit être détecté dans "+ FunSets.toString(set2and3))
      
      assert(!exists(s1, evenOnly), "aucun pair")

    }
  }
  
  test("Map ") {
    new TestSets {
      def incOne : Int => Int = x => x + 1
      def decOne : Int => Int = x => x - 1
      def toZero : Int => Int = x => 0

      val zz = map(s1, incOne)
      println("contains == "+contains(zz, 2))
      assert(contains(zz, 2), "s1 + 1 contains 2")
      assert(contains(map(s2, decOne), 1), "s2 - 1 contains 1")
      assert(contains(map(s2, toZero), 0), "s2 to 0 contains 0")
      assert(!contains(map(s2, toZero), 2), "s2 to 0 not contains 2")

    }
  }
}
