package objsets

import org.scalatest.FunSuite
import objsets.TweetReader
import objsets.GoogleVsApple
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }
  
  test("filter: accept all") {
    new TestSets {
      assert(size(set5.filter(tw => true)) === 4)
    }
  }
  
  test("filter: accept all on empty list") {
    new TestSets {
      assert(size(new Empty().filter(tw => true)) === 0)
    }
  }
  test("filter: accept none") {
    new TestSets {
      assert(size(set5.filter(tw => false)) === 0)
    }
  }
  test("filter: right and left branch are filtered") {
    new TestSets {
      val e = new Empty
      val b = e.incl(new Tweet("a", "AA", 20)).incl(new Tweet("b", "BB", 20))
      assert(size(b.filter(tw => tw.retweets == 20)) === 2)
    }
  }    
  
 test("filter: right branch") {
    new TestSets {
      val e = new Empty
      val b = e.incl(new Tweet("a", "AA", 20)).incl(new Tweet("b", "BB", 20))
      val data = b.incl(new Tweet("aa", "AAa", 20)).incl(new Tweet("bb", "BBb", 20))
        .incl(new Tweet("aaa", "AAaa", 322)).incl(new Tweet("bbb", "BBbb", 321))
      println(data)
      assert(size(data.filter(tw => tw.retweets == 321)) === 1)
    }
  }   
 test("filter: left branch") {
    new TestSets {
      val e = new Empty
      val data = e
      	.incl(new Tweet("4a", "4a", 4))
      	.incl(new Tweet("5e", "5e", 5))
      	.incl(new Tweet("5B", "5B", 5))
      	.incl(new Tweet("3b", "3b", 3))
      	.incl(new Tweet("6a", "6a", 6))
      	.incl(new Tweet("6b", "6b", 6))
      	.incl(new Tweet("6", "6", 6))
      	.incl(new Tweet("1C", "1C", 1))
      	.incl(new Tweet("4b", "4b", 4))
      	.incl(new Tweet("3", "3", 3))
      	.incl(new Tweet("2", "2", 2))
      	.incl(new Tweet("4d", "4d", 4))
      	.incl(new Tweet("4c", "4c", 4))
      	.incl(new Tweet("2a", "2a", 2))
      	.incl(new Tweet("2b", "2b", 2))
      	.incl(new Tweet("1", "1", 1))
      	.incl(new Tweet("1b", "1B", 1))
      	.incl(new Tweet("3a", "3a", 3))
      	.incl(new Tweet("ZZ", "ZZ", 900))
      	.incl(new Tweet("  ", " ", 800))

      println(data)
      assert(size(data.filter(tw => tw.retweets == 4)) === 4)
      assert(size(data.filter(tw => tw.retweets >= 4)) === 11)
      assert(size(data.filter(tw => tw.retweets == 3)) === 3)
      assert(size(data.filter(tw => tw.retweets <= 0)) === 0)
      assert(size(data.filter(tw => tw.retweets == 900)) === 1)
      assert(size(data.filter(tw => tw.retweets == 800)) === 1)

    }
  }  
 
 test("filter: google dataset") {
    new TestSets {
      assert(size(GoogleVsApple.appleTweets.filter(tw => tw.retweets == 321)) ===1)
    }
 }
  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set as right") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set as left") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted : empty set should throw ex") {
       new TestSets {
         intercept[NoSuchElementException] {
           set1.mostRetweeted
         }
       }
   }
  
  test("most retweeted : singleton set") {
       new TestSets {
          val tweet = new Tweet("a", "a body", 20)
          val singleton = set1.incl(tweet)
          assert(singleton.mostRetweeted === tweet)
       }
   }
   test("most retweeted : 2 itemsset with first as good answer") {
       new TestSets {
          val retweeted = new Tweet("a", "a body", 20)
          val justOne = new Tweet("a", "a body", 1)
          assert(set1.incl(retweeted).incl(justOne).mostRetweeted === retweeted)
       }
   }
   test("most retweeted : 2 itemsset with last as good answer") {
       new TestSets {
          val retweeted = new Tweet("a", "a body", 20)
          val justOne = new Tweet("b", "b body", 1)
          val s = set1.incl(justOne).incl(retweeted)
          assert(s.mostRetweeted === retweeted)
       }
   }
    test("most retweeted : s3 is 20") {
       new TestSets {
          assert(set3.mostRetweeted.retweets === 20)
       }
   }
    
  test("descending: empty set generate an empty list ") {
    new TestSets {
      assert(set1.descendingByRetweet.isEmpty)
    }
  }
  test("descending: singleton set generate an singleton list ") {
    new TestSets {
      val tweet = new Tweet("a", "a body", 20)
      val singleton = set1.incl(tweet)
      val trends = singleton.descendingByRetweet

      assert(!trends.isEmpty, "empty trends not expected: "+trends)
      assert(trends.head === tweet)
      assert(trends.tail.isEmpty, "tail is not empty")
    }
  }
  test("descending: multiset") {
    new TestSets {
      val t10 = new Tweet("a", "10 body", 10)
      val t20= new Tweet("a", "20 body", 20)
      val t30= new Tweet("a", "30 body", 30)

      val set = set1.incl(t10).incl(t20).incl(t30)
      val trends = set.descendingByRetweet
//      println("trends:")
//      trends foreach println

      assert(!trends.isEmpty, "empty trends not expected: "+trends)
      assert(trends.head === t30)
      assert(!trends.tail.isEmpty, "1st tail is empty")
      assert(trends.tail.head === t20)
      assert(!trends.tail.tail.isEmpty, "2nde tail is empty")
      assert(trends.tail.tail.head === t10)
      assert(trends.tail.tail.tail.isEmpty, "3rd tail should be empty")
    }
  }   

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet

      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  test("big") {
     new TestSets {
    	 println("Big")
    	 println("Apple:"+GoogleVsApple.appleTweets)
    	 println("Google:"+GoogleVsApple.googleTweets)
    	 println("Trending : "+GoogleVsApple.trending)
    	 GoogleVsApple.trending foreach println
    	 assert(true)
     }
  }
}
