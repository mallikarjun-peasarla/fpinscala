package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

//  def toList: List[A] = {
//    def go(s: => Stream[A]): List[A] = this match {
//      case Empty => List()
//      case Cons(h, t) => h() :: go(t())
//    }
//    go(this)
//  }

  def toList: List[A] = this match {
    case Empty =>
      List[A]()
    case Cons(h: A, t) =>
      h() :: t().toList
  }

  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => cons[A](h(), go(t(), n-1))
    }
    go(this, n)
  }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def main(args: Array[String]): Unit = {
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = if (cond) onTrue() else onFalse()
    val a = 0
    if2(a < 22, () => println("a"), () => println("b"))
    // if2(false, sys.error("fail"), () => print("done"))

    def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
    val x = maybeTwice(true, { println("hi"); 1+41 })
    println(x)

    println("\nlazy testing:")
    def maybeTwice2(b: Boolean, i: => Int) = {
      lazy val j=i;
      maybeTwice(b, j)
    }
    println("true- ")
    println(maybeTwice2(true, { println("hi"); 1+41 }))
    println("false- ")
    println(maybeTwice2(false, { println("hi"); 1+41 }))

    // Streams
    val stream1 = Stream.apply(1,2,3,4)
    println("stream: "+stream1)
    println("stream.toList: "+stream1.toList)
    println("stream.take(3).toList: "+stream1.take(3).toList)
  }
}