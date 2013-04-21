package objsets

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(173); 
  def max(a: Tweet, b: Tweet): Tweet = {
    if (a == null) b
    else if (b == null) a
    else if (a.retweets > b.retweets) a
    else b
  };System.out.println("""max: (a: objsets.Tweet, b: objsets.Tweet)objsets.Tweet""");$skip(47); 
  val retweeted = new Tweet("a", "a body", 20);System.out.println("""retweeted  : objsets.Tweet = """ + $show(retweeted ));$skip(44); 
  val justOne = new Tweet("a", "a body", 1);System.out.println("""justOne  : objsets.Tweet = """ + $show(justOne ));$skip(18); val res$0 = 
  max(null, null);System.out.println("""res0: objsets.Tweet = """ + $show(res$0));$skip(23); val res$1 = 
  max(retweeted, null);System.out.println("""res1: objsets.Tweet = """ + $show(res$1));$skip(21); val res$2 = 
  max(justOne, null);System.out.println("""res2: objsets.Tweet = """ + $show(res$2));$skip(22); val res$3 = 
  max(null,  justOne);System.out.println("""res3: objsets.Tweet = """ + $show(res$3));$skip(27); val res$4 = 
  max(retweeted,  justOne);System.out.println("""res4: objsets.Tweet = """ + $show(res$4))}
  
}
