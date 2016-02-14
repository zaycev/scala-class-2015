import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

case class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}


class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new scala.NoSuchElementException("Nil.tail")
}


def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])


println(singleton(1))
println(singleton(true))


def nth[T](list: List[T], index: Int): T = {
  if (list.isEmpty || index < 0)
    throw new IndexOutOfBoundsException()
  else
    if (index == 0)
      list.head
    else
      nth(list.tail, index - 1)
}

val l = new Cons(1, new Cons(2, singleton(3)))

nth(l, 0)
nth(l, 1)
nth(l, 2)
nth(l, 3)
nth(l, -1)

