package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a: A) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a: A) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a: A) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a: A) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    val optM = mean(xs)
    optM.flatMap(m => mean(xs.map(n => math.pow(n - m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f  // a: A => a.map()

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  def map21[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a1 <- a   // flatMap
      b1 <- b   // flatMap
    } yield f(a1, b1) // map

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft(Some(List[A]())) {
      case (Some(acc), Some(v)) => Some(v :: acc)
      case (Some(acc), None) => Some(acc)
      case _ => Some(Nil)
    }

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(l.map(a => f(a)))

  def main(args: Array[String]): Unit = {
    // failingFn(5)
    failingFn2(5)
    println(absO(Some(2.1)))

    val op1 = Some("bread")
    val op2 = Some("jam")
    val op3 = None
    val and = (s1: String, s2: String) => s1 + " and " + s2
    println("map2: "+map2(op1, op2)(and))
    println("map2 with a None: "+map2(op1, op3)(and))

    val emptyString = (s: String) => if(s.isEmpty) None else Some(s)
    println("sequence: "+sequence(List(op1, op3, op2)))
    println("sequence of empty list: "+sequence(List()))

    println("traverse: "+traverse[String, String](List("str1", "", "str2"))(emptyString))
    println("traverse empty strings list: "+traverse[String, String](List("", "", ""))(emptyString))
  }
}