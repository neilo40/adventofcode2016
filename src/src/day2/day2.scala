import scala.io.Source

case class GuardedPair(_row: Int, _col: Int) {
  val row = guardedVal(_row)
  val col = guardedVal(_col)

  def guardedVal(value: Int): Int = value match {
    case v if v < 0 => 0
    case v if v > 2 => 2
    case _ => value
  }
}

object day2 {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("src/inputs/day2.txt").getLines.toList
    var startingPosition = GuardedPair(1, 1)
    for (instruction <- instructions) {
      startingPosition = processMoves(startingPosition, instruction.toList)
      val keypad = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
      println(keypad(startingPosition.row)(startingPosition.col))
    }
  }

  def processMoves(position: GuardedPair, moves: List[Char]): GuardedPair = {
    moves match {
      case Nil => position
      case direction :: tail => processMoves(newPosition(position, direction), tail)
    }
  }

  def newPosition(position: GuardedPair, direction: Char): GuardedPair = {
    direction match {
      case 'U' => GuardedPair(position.row - 1, position.col)
      case 'D' => GuardedPair(position.row + 1, position.col)
      case 'L' => GuardedPair(position.row, position.col - 1)
      case 'R' => GuardedPair(position.row, position.col + 1)
      case _ => position
    }
  }
}
