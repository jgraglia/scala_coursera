package patmat

trait TestList[T] {
  def head: T
  def tail: TestList[T]
  def isEmpty: Boolean
  def foreach(f: T => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

class Nil[T] extends TestList[T] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons[T](val head: T, val tail: TestList[T]) extends TestList [T] {
  def isEmpty = false
}
//
//object List {
//  def  apply[T](): TestList[T] {
//    new Nil[T]()
//  }
//
//  def  apply[T](a:T, b:T): TestList[T] {
//    new Cons(a, new Cons(b, new Nil()))
//  }
//}



