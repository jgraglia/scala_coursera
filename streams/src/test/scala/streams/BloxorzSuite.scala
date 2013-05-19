package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  trait SingleLine extends SolutionChecker {
      /* terrain for level 1*/

    val level ="ooo-------".stripMargin
  }
  
  test("block standing") {
    new Level1 {
      val block = Block(Pos(0,0), Pos(0,0))
      assert(block.isStanding === true)
	  assert(block.down.isStanding === false)
	  assert(block.right.isStanding === false)
	  assert(block.right.right.isStanding === true)
    }
  }
  
  test("block not standing") {
    new Level1 {
      val block = Block(Pos(0,0), Pos(0,1))
      assert(block.isStanding === false)
	  assert(block.right.isStanding === true)
      assert(block.down.isStanding === false)
	  assert(block.down.right.isStanding === true)
    }
  }  
  test("first cube out of terrain : row") {
    new SingleLine {
      assert( Block(Pos(0,0), Pos(0,1)).isLegal === true)
      assert( Block(Pos(1,0), Pos(1,1)).isLegal === false)
    }
  }  
  test("last cube out of terrain : row") {
    new SingleLine {
      assert( Block(Pos(0,0), Pos(1,0)).isLegal === false)
    }
  }    
   test("in terrain") {
    new SingleLine {
      assert(terrain(Pos(0,2))===true)
      assert(terrain(Pos(0,38))===false," should not be in terrain 4 ")
      assert(terrain(Pos(0,3))===false," should not be in terrain 3")
    }
  }  
  test("first cube out of terrain : col") {
    new SingleLine {
      assert(terrain(Pos(0,1)))
      assert(terrain(Pos(0,2)))
      assert(terrain(Pos(0,3))===false," should not be in terrain ")
      assert( Block(Pos(0,1), Pos(0,2)).isLegal === true)
      assert( Block(Pos(0,2), Pos(0,3)).isLegal === false)
    }
  }    
  test("last cube out of terrain : col") {
    new Level1 {
      assert( Block(Pos(3,1), Pos(3,2)).isLegal === true)
      assert( Block(Pos(3,0), Pos(3,1)).isLegal === false)
    }
  }    
  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  test("all neighbors") {
    new Level1 {
      assert(startBlock.neighbors.size === 4)
    }
  }

  test("legalNeighbors for start position") {
    new Level1 {
      assert(startBlock.legalNeighbors.size === 2)
    }
  }
}
