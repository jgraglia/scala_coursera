package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: no change when no money") {
    assert(countChange(0, List(1, 2)) === 0)
  }
  test("countChange: no change if negative money") {
    assert(countChange(-10, List(1, 2)) === 0)
  }
  test("countChange: no change when no coin") {
    assert(countChange(1, List()) === 0)
  }
  test("countChange: first coin is enough") {
    assert(countChange(6, List(6, 5, 4)) === 1)
  }
  test("countChange: need same coin again") {
    assert(countChange(12, List(6, 5)) === 1)
  }
  test("countChange: need same coin again and again") {
    assert(countChange(24, List(6, 5)) === 1)
  }
  test("countChange: need only the other coin") {
    assert(countChange(5, List(6, 5)) === 1)
  }
  test("countChange: need only the other coin twice") {
    assert(countChange(10, List(6, 5)) === 1)
  }
  test("countChange: need one of each kind") {
    assert(countChange(11, List(6, 5)) === 1)
  }
  test("countChange: no change when minimum coin is greater than the amount") {
    assert(countChange(1, List(6, 5, 4, 3, 2, 2, 2, 2)) === 0)
  }
  test("countChange: only one way") {
    assert(countChange(5, List(3,2)) === 1)
  }
  
  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }
}
