package fpinscala

object TestUtils {

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

  val generateLists = (x: Int) => x match {
    case 1 => List(1)
    case 2 => List(2,2)
    case 3 => List(3,3,3)
    case 4 => List(4,4,4,4)
  }

}
