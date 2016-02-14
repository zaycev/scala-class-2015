object session {

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

  object List {

    def apply[T](): List[T] = new Nil
    def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
    def apply[T](x1: T, x2: T, x3: T): List[T] = new Cons(x1, new Cons(x2, new Cons(x3, new Nil)))

  }


  List()
  List(1)
  List(1, 2)
  List(1, 2, 3)

}