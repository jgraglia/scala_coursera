package funsets

object Main extends App {
  import FunSets._
  
   def sum(f: Int => Int,a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
           if (a > b) acc
           else loop(a+1,acc+f(a))
        }
        loop(a, 0)
   }

  def id(x:Int) :Int = {x}
  println(sum((x: Int) => x, 1, 3))
  println(id(3))

  println(contains(singletonSet(1), 1))
}  
 
