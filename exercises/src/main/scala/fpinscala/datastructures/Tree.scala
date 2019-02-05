package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(n) => acc
      case Branch(l, r) => go(r, go(l, acc+1)+1)
    }
    go(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    def go(t: Tree[Int], currMax: Int): Int = t match {
      case Leaf(n) => n max currMax
      case Branch(l, r) => go(r, go(l, currMax))
    }
    go(t, 0)
  }

  def depth[A](t: Tree[A]): Int = {
    def go(t: Tree[A], depth: Int, maxDepth: Int): Int = t match {
      case Leaf(n) => depth max maxDepth
      case Branch(l, r) => go(r, depth + 1, go(l, depth + 1, maxDepth))
    }
    go(t, 0, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    def go(t: Tree[A]): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(go(l), go(r))
    }
    go(t)
  }

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(9)))), Branch(Leaf(4), Branch(Leaf(5), Leaf(6))))
    println("tree size: "+size(t))
    println("max in tree: "+maximum(t))
    println("depth of tree: "+depth(t))

    val intToString = (x: Int) => x match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
    }
    println("map transform: "+map[Int, String](t)(intToString))
  }

}