package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: empty") {
    assert(wordOccurrences("") === List())
  }
  
  test("wordOccurrences: a") {
    assert(wordOccurrences("a") === List(('a', 1)))
  }
  test("wordOccurrences: A") {
    assert(wordOccurrences("A") === List(('a', 1)))
  }
   test("wordOccurrences: aa") {
    assert(wordOccurrences("aa") === List(('a', 2)))
  }
  test("wordOccurrences: ignore case aA") {
    assert(wordOccurrences("aA") === List(('a', 2)))
  }
  test("wordOccurrences: sorted") {
    assert(wordOccurrences("ba") === List(('a', 1),('b',1)))
  }
  test("wordOccurrences: sorted on char only") {
    assert(wordOccurrences("bab") === List(('a', 1),('b',2)))
  }
  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  // ==============
  test("sentenceOccurrences: empty") {
    assert(sentenceOccurrences(List()) === List())
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }
test("sentenceOccurrences: keep order among words") {
    assert(sentenceOccurrences(List("e", "abcd")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }
  test("sentenceOccurrences: abcd e with empty  ") {
    assert(sentenceOccurrences(List("abcd", "", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  // ==============
  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }
test("dictionaryByOccurrences.get: abbey  multiple occurences") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('b', 2), ('e', 1), ('y', 1))).map(_.toSet) === Some(Set("abbey")))
  }

test("dictionaryByOccurrences.get: Aarhus case") {
    assert(dictionaryByOccurrences.get(List(('a', 2), ('h', 1), ('r', 1), ('s', 1), ('u', 1))).map(_.toSet) === Some(Set("Aarhus")))
  }
  // ==============

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams: unknownnnn") {
    assert(wordAnagrams("unknownnnn").toSet === Set())
  }
  
  test("word anagrams: empty string") {
    assert(wordAnagrams("").toSet === Set())
  }

  test("word anagrams: eat") {
    assert(wordAnagrams("eat").toSet === Set("ate", "eat", "tea"))
  }
  // ==============

  test("combinations: empty list") {
    assert(combinations(List()).contains(List()))
    assert(combinations(List()).size === 1)
  }
  test("combinations: always contains empty list") {
    assert(combinations(List(('a', 2), ('b', 2))).contains(List()))
    assert(combinations(List(('a', 2), ('b', 2))).size > 1)
  }
  test("combinations: single char combination") {
	   assert(combinations(List(('a', 1))).contains(List(('a', 1))), "single char combination not found")
    assert(combinations(List(('a', 1))).contains(List()), "missing empty list")
    assert(combinations(List(('a', 1))).size === 2)
  }
  
  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: empty occurrence") {
    assert(combinations(List(('a', 0), ('b', 0))).contains(List()))
    assert(combinations(List(('a', 0), ('b', 0))).size === 1)
  }
  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
   // println("Obtained : "+combinations(abba))
    //println("Expected : "+abbacomb)
    assert(combinations(abba).toSet === abbacomb.toSet)
  }  
  // ==============

  test("subtract: emptyList  - emptyList") {
    assert(subtract(List(), List()) === List())
  }
  test("subtract: anything  - emptyList") {
       val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
       assert(subtract(lard, List()) === lard)
  }
  ignore("subtract: emptyList  - anything") {
	  // pas possible selon la doc
  }
  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }
  
  test("subtract: lard - rr") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val rr = List(('r', 2))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, rr) === lad)
  }
test("subtract: unknown item : lard - rrxx") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val rrxx = List(('r', 2),('x', 2))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, rrxx) === lad)
  }





  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }  

}
