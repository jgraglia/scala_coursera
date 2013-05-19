package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._
import streams._

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {
  trait StringParserChecker extends GameDef with Solver with StringParserTerrain {
    var level1 = terrainFunction(Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o')))
  }
  test("level with no cell") {
    new StringParserChecker   {
      val level = ""
      assert(terrainFunction(Vector(Vector()))(Pos(1,1)) === false)
    }
  }
  test("cell out of bound") {
    new StringParserChecker   {
      val level = ""
      assert(level1(Pos(10,1)) === false)
      assert(level1(Pos(0,10)) === false)
    }
  }
    test("cell invalid") {
    new StringParserChecker   {
      val level = ""
      assert(level1(Pos(0,-1)) === false)
      assert(level1(Pos(-1,0)) === false)
    }
  }
  test("cell valid") {
    new StringParserChecker   {
      val level = ""
      assert(level1(Pos(0,0)) === true)
      assert(level1(Pos(0,1)) === true)
      assert(level1(Pos(1,0)) === true)
      assert(level1(Pos(1,1)) === true)
      assert(level1(Pos(2,0)) === true)
      assert(level1(Pos(2,1)) === true)
      assert(level1(Pos(0,2)) === false, "2 col not exists")
      assert(level1(Pos(1,2)) === false)
      assert(level1(Pos(2,2)) === false)
    }
  }
  
  test("cell not found") {
    new StringParserChecker   {
      val level = ""
      intercept[IllegalArgumentException] {
    	  findChar('c', Vector(Vector()))
      }
    }
  }
  
  test("first cell match") {
    new StringParserChecker   {
      val level = ""
      assert(findChar('c', Vector(Vector('c'))) === Pos(0,0))
    }
  }
  test("last cell of row") {
    new StringParserChecker   {
      val level = ""
      assert(findChar('m', Vector(Vector('c','j','k','l','m'))) === Pos(0,4))
    }
  }
  
  test("another row") {
    new StringParserChecker   {
      val level = ""
      assert(findChar('b', Vector(Vector('c','j','k','l','m'),Vector('n','b','k','l','m'))) === Pos(1,1))
    }
  }
  
  test("use first occurence") {
    new StringParserChecker   {
      val level = ""
      assert(findChar('b', Vector(Vector('c','j','k','l','m'),Vector('n','b','b','l','m'))) === Pos(1,1))
    }
  }  
}
