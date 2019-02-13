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
    case Cons(_, _) if(n == 0) => empty
    case Cons(h, t) => cons[A](h(), t().take(n-1))
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this) {
      case Cons(_, _) if(n == 0) => None
      case Cons(h, t) => Some(h(), t().take(n-1))
    }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n == 0) => cons[A](h(), t())
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons[A](h(), t().takeWhile(p))
    case Cons(h, t) => empty
  }

  // f: (A, => B) => B
  //cannot terminate when function not true with fold
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if(p(a)) cons(a, b) else b)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if(p(h())) => Some(h(), t())
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((e,a) => p(e) && a)

  def forAll1(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if(p(h())) => true && t().forAll(p)
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] = foldRight(None: Option[A]){
    case (a: A, _) => Some(a)
  }

  // Part of the exercise is writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

  def map1[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => cons(f(h()), t().map1(f))
  }

  /* def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] -- S=Stream[B]
    -~->  def unfold[Stream[B]](a: B)(f: B => Option[(B, Stream[B])]): Stream[B] */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) =>
        Some((f(h()), t()))
      case _ => None
    }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if(p(h)) cons(h, t) else t)

  def filterViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if(p(h())) => Some(h(), t())
      case _ => None
    }

  def append[B>:A](s: Stream[B]): Stream[B] = foldRight(s)((e, acc) => cons(e, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =  foldRight(empty[B])((h,t) => f(h) append t)

  def appendElement[B>:A](a: B): Stream[B] = cons(a, this)

  def zipWithViaUnfold[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this,b)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  }

  def zipAllViaUnfold[B](b: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this,b)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(),t2()))
    case (Empty, Cons(h2,t2)) => Some((None,Some(h2())), (empty,t2()))
    case (Cons(h1,t1), Empty) => Some((Some(h1()),None), (t1(),empty))
    case _ => None
  }

  def startsWith[B>:A](s: Stream[B]): Boolean =
    zipWithViaUnfold(s)((a1: B, a2:B) => a1 == a2).
      foldRight(true)((b1, b2) => b1 && b2)

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((t(), t()))
    case _ => None
  }

  def tailsWithSelf: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((Cons(h, t), t()))
    case _ => None
  }

  // This small example, of assembling hasSubsequence from simpler functions using laziness, is from Cale Gibbard.
  // See this post: http://lambda-the-ultimate.org/node/1277#comment-14313.
  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  // def scanRight[B](b1: B)(f: (A,A) => B) : Stream[B] = tails.map(s => s.foldRight(b1){(b,acc) => f(b)})
  def scanRight[B](b1: B)(f: (A, B) => B) : Stream[B] = tailsWithSelf.map(s => s.foldRight(b1){(a,acc) => f(a, acc)})

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
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def fibs: Stream[Int] = {
    def go(prev: Int, curr: Int): Stream[Int] = Stream.cons(curr, go(curr, prev + curr))
    go(1, 0)
  }

  // unfold is corecursive function. A recursive function consumes data, a corecursive function produces data.
  // Corecursion is also sometimes called guarded recursion, and productivity is also sometimes called cotermination.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) =>
      Stream.cons(a, unfold(s)(f))
    case None => empty
  }

  val onesViaUnfold: Stream[Int] = unfold(1){case (n: Int) => Some((n,n))}
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n){case (n: Int) => Some((n,n+1))}
  /* def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
      -~->  def unfold[A](a: A)(f: A => Option[(A, A)]): Stream[A] */
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a){case (a: A) => Some((a,a))}
  // def fibsViaUnfold: Stream[Int] = unfold((0,1))((prev: Int, curr: Int) => Some((prev, (curr, prev + curr))))
  def fibsViaUnfold: Stream[Int] = unfold((0,1)){case (prev: Int, curr: Int) => Some((prev, (curr, prev + curr)))}

  def zipWith[A,B,C](a:Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] = (a,b) match {
    case (Empty, _) => empty
    case (_, Empty) => empty
    case (Cons(h1,t1), Cons(h2,t2)) => Stream.cons(f(h1(),h2()), zipWith(t1(),t2())(f))
  }

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
    println()

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
    println()

    import fpinscala.TestUtils.intToString
    println("stream.map(intToString).toList: "+stream1.map(intToString).toList)
    println("stream.mapViaUnfold(intToString).toList: "+stream1.mapViaUnfold(intToString).toList)
    println("stream.map(_ * 5).toList: "+stream1.map(_ * 5).toList)
    println("stream.filter(_ % 2 == 0).toList: "+stream1.filter(_ % 2 == 0).toList)
    println("stream.filterViaUnfold(_ % 2 == 0).toList: "+stream1.filterViaUnfold(_ % 2 == 0).toList)

    def genStreams(x: Int) = {
      def go(n: Int): Stream[Int] = if(n == 0) Empty else cons(x, go(n-1))
      go(x)
    }
    val intStreams = (x: Int) => genStreams(x)
    println("stream.flatMap(intStreams).toList: "+stream1.flatMap(intStreams).toList)
    println()

    println("ones.map(_ + 1).exists(_ % 2 == 0): "+ones.map(_ + 1).exists(_ % 2 == 0))
    println("ones.takeWhile(_ == 1): "+ones.takeWhile(_ == 1))
    println("ones.forAll(_ != 1): "+ones.forAll(_ != 1))
    // println("ones.forAll(_ == 1): "+ones.forAll(_ == 1))
    println("constant(\"same\").take(5).toList: "+constant("same").take(5).toList)
    println("constantViaUnfold(\"same\").take(5).toList: "+constantViaUnfold("same").take(5).toList)
    println("from(11).take(5).toList: "+from(11).take(5).toList)
    println("fromViaUnfold(11).take(5).toList: "+fromViaUnfold(11).take(5).toList)
    println("fibs.take(15).toList: "+fibs.take(15).toList)
    println("fibsViaUnfold.take(15).toList: "+fibsViaUnfold.take(15).toList)
    println()

    val stream2 = Stream.apply(4,3,2,1)
    println("stream1.zipWith(stream2)({case (a,b) => a + b}).toList: "+zipWith(stream1, stream2)({case (a,b) => a + b}).toList)
    println("stream1.zipWithViaUnfold(stream2)({case (a,b) => a + b}).toList: "+stream1.zipWithViaUnfold(stream2)({case (a,b) => a + b}).toList)
    println("stream1.startsWith(Stream.apply(1,2,3)): "+stream1.startsWith(Stream.apply(1,2,3)))
    println("stream1.startsWith(Stream.apply(1,2,4)): "+stream1.startsWith(Stream.apply(1,2,4)))
    println("stream1.zipAllViaUnfold(Stream.apply(1,2,4)).toList: "+stream1.zipAllViaUnfold(Stream.apply(1,2,3)).toList)
    println("Stream.apply(1,2,3).zipAllViaUnfold(stream1).toList: "+Stream.apply(1,2,3).zipAllViaUnfold(stream1).toList)
    println("stream1.tails (list view): ")
    stream1.tails.toList.foreach(s => print(s.toList+","))
    println()
    println("stream1.scanRight: "+stream1.scanRight(0)(_ + _).toList)
  }
}