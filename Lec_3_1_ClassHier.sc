abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false
  def incl(x: Int): IntSet = new NonEmpty(x)
  def union(other: IntSet) = other
  override def toString = ""
}

class NonEmpty(elem: Int, left: IntSet = Empty, right: IntSet = Empty) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = s"{$left$elem$right}"

}


val s = new NonEmpty(3)
val s1 = s incl 1 incl 2
val s2 = s incl 4 incl 5
val s4 = s1 union s2

s4 contains 5


