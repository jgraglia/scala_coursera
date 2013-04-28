object funcint {
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}
object Zero extends Nat {
  override def isZero = true;
  override def predecessor: Nat = throw new IllegalStateException()
  override def successor: Nat = new Succ(this)
  override def + (that: Nat): Nat = that
  override def - (that: Nat): Nat = throw new IllegalStateException()
  override def toString()= "0"
}
class Succ(n: Nat) extends Nat {
  override def isZero = false;
  override def predecessor: Nat = n
  override def successor: Nat = new Succ(this)
  override def + (that: Nat): Nat = {
     def plus(acc:Nat, remaining:Nat):Nat= {
     	if (remaining.isZero) acc
     	else plus(acc.successor, remaining.predecessor)
     }
  	if (that.isZero) this
  	else plus(successor, that.predecessor)
  }
  override def - (that: Nat): Nat = {
     def minus(acc:Nat, remaining:Nat):Nat= {
     	if (remaining.isZero) acc
     	else minus(acc.predecessor, remaining.predecessor)
     }
  	if (that.isZero) this
  	else minus(predecessor, that.predecessor)
  }
  override def toString()= "1+"+n
};import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1170); val res$0 = 

Zero.successor;System.out.println("""res0: funcint.Nat = """ + $show(res$0));$skip(14); 
println(Zero);$skip(34); 
println(Zero.successor.successor);$skip(43); 
assert(Zero == Zero.successor.predecessor);$skip(25); 
val one = Zero.successor;System.out.println("""one  : funcint.Nat = """ + $show(one ));$skip(24); 
val two = one.successor;System.out.println("""two  : funcint.Nat = """ + $show(two ));$skip(26); 
val three = two.successor;System.out.println("""three  : funcint.Nat = """ + $show(three ));$skip(28); 

assert(one == Zero + one );$skip(26); 
assert(Zero == one - one);$skip(26); 
assert(one == two - one );$skip(21); 
println((one + one));$skip(53); 
assert(Zero == (one + one).predecessor.predecessor );$skip(17); 

println("OKay")


trait Expr
case class Number(x:Int) extends Expr;
case class Sum(l:Expr, r:Expr) extends Expr;
case class Prod(l:Expr, r:Expr) extends Expr;$skip(282); ;

def eval(e:Expr):Int = e match{
    case Number(x) => x
    case Sum(l,r) => eval(l)+eval(r)
    case Prod(l,r) => eval(l)*eval(r)
    
};System.out.println("""eval: (e: funcint.Expr)Int""");$skip(367); 
def show(e:Expr):String = e match{
    case Number(x) => x.toString()
    case Sum(l,r) => show(l)+"+"+show(r)
    case Prod(Number(a),Number(b)) => show(Number(a))+"*"+show(Number(b))
    case Prod(Number(a),r) => show(Number(a))+"*("+show(r)+")"
    case Prod(l,Number(a)) => "("+show(l)+")*"+show(Number(a))
    case Prod(l,r) => "("+show(l)+") * ("+show(r)+")"
};System.out.println("""show: (e: funcint.Expr)String""");$skip(51); 
val e  =Prod(Sum(Number(1), Number(2)), Number(9));System.out.println("""e  : funcint.Prod = """ + $show(e ));$skip(20); val res$1 = 
show(e)+"="+eval(e);System.out.println("""res1: String = """ + $show(res$1));$skip(32); val res$2 = 
show(Sum(Number(1), Number(2)));System.out.println("""res2: String = """ + $show(res$2));$skip(48); val res$3 = 
show(Sum(Number(1), Sum(Number(2), Number(3))));System.out.println("""res3: String = """ + $show(res$3))}

/*
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() =>  List(x)
  case y :: ys => {
    if (ys.isEmpty()) y :: x
    else y::  insert(x, ys.tail)
	  /*if (x<y) List(y,  ys.head :: insert(x, ys.tail))
	  else if (x == y) List(y, x, ys)
	  else List(ys)
	  */
  }
}
insert(9, Nil)
var a = 1::2::6::Nil
insert(4, a)
println(a.toString())
*/
}
