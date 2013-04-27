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

println("OKay")}
}
