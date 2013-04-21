package objsets

object test {
  def max(a: Tweet, b: Tweet): Tweet = {
    if (a == null) b
    else if (b == null) a
    else if (a.retweets > b.retweets) a
    else b
  }                                               //> max: (a: objsets.Tweet, b: objsets.Tweet)objsets.Tweet
  val retweeted = new Tweet("a", "a body", 20)    //> retweeted  : objsets.Tweet = User: a
                                                  //| Text: a body [20]
  val justOne = new Tweet("a", "a body", 1)       //> justOne  : objsets.Tweet = User: a
                                                  //| Text: a body [1]
  max(null, null)                                 //> res0: objsets.Tweet = null
  max(retweeted, null)                            //> res1: objsets.Tweet = User: a
                                                  //| Text: a body [20]
  max(justOne, null)                              //> res2: objsets.Tweet = User: a
                                                  //| Text: a body [1]
  max(null,  justOne)                             //> res3: objsets.Tweet = User: a
                                                  //| Text: a body [1]
  max(retweeted,  justOne)                        //> res4: objsets.Tweet = User: a
                                                  //| Text: a body [20]
  
}