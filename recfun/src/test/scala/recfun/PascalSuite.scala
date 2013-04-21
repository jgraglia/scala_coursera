package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("need a positive row") {
    intercept[IllegalArgumentException] {
    	pascal(1,-2)
    }
  } 
  test("need a positive column") {
    intercept[IllegalArgumentException] {
    	pascal(-1,2)
    }
  } 
  test("must target a cell of the triangle") {
    intercept[IllegalArgumentException] {
    	pascal(10,2)
    }
  } 
  
  test("first col of a row is always equals to 1") {
    assert(pascal(0,0) === 1)
    assert(pascal(0,1) === 1)
    assert(pascal(0,2) === 1)
    assert(pascal(0,103) === 1)
  }
  test("last col of a row is always equals to 1") {
    assert(pascal(0,0) === 1)
    assert(pascal(1,1) === 1)
    assert(pascal(2,2) === 1)
    assert(pascal(103,103) === 1)
  }
  test("on row 2, middle column is equals to 2") {
    assert(pascal(1,2) === 2)
  }  
  
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
}
