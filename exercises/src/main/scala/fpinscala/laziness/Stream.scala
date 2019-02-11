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

  def exists1(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList1: List[A] = {
    def go(s: => Stream[A]): List[A] = s match {
      case Empty => List()
      case Cons(h, t) => h() :: go(t())
    }
    go(this)
  }

  def toList: List[A] = this match {
    case Empty =>
      List[A]()
    case Cons(h: A, t) =>
      h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(_, _) if(n == 0) => Empty
    case Cons(h, t) => cons[A](h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n == 0) => cons[A](h(), t())
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons[A](h(), t().takeWhile(p))
    case Cons(h, t) => Empty
  }

  // f: (A, => B) => B
  //todo: terminate when function not true ?
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if(p(a)) cons(a, b) else b)

  def forAll1(p: A => Boolean): Boolean = this.foldRight(true)((e,a) => p(e) && a)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if(p(h())) => true && t().forAll(p)
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] = foldRight(None: Option[A]){
    case (a: A, _) => Some(a)
  }

  // todo: 5.7 map, filter, append, flatmap using foldRight.
  // Part of the exercise is writing your own function signatures.

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
    println("stream.toList: "+stream1.toList1)
    println("stream.take(3).toList: "+stream1.take(3).toList)
    println("stream.drop(3).toList: "+stream1.drop(3).toList)
    println("stream.drop(2).toList: "+stream1.drop(2).toList)
    println("stream.takeWhile(_ < 3).toList: "+stream1.takeWhile(_ < 3).toList)
    println("stream.takeWhile2(_ < 3).toList: "+stream1.takeWhile2(_ < 3).toList)
    println("stream.forAll(_ < 5): "+stream1.forAll(_ < 5))
    println("stream.forAll(_ % 2 == 0): "+stream1.forAll(_ % 2 == 0))
    println("stream.headOption: "+stream1.headOption)
    println("emptyStream.headOption: "+Stream.apply().headOption)
  }
}