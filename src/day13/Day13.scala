package day13

case class Coordinate(x: Int, y: Int)

object Day13 {
  def main(args: Array[String]): Unit = {
    step(Coordinate(1, 1), Coordinate(1, 1), 0, List.empty)
  }

  def step(previousStep: Coordinate, thisStep: Coordinate, distance: Int, been: List[Coordinate]): Unit = {
    thisStep match {
      case x if isDestination(x) => println(s"Destination ($thisStep) found after $distance")
      case _ =>
        val n = nextSteps(thisStep, previousStep)
        n match {
          case y if y.nonEmpty =>
            n.filter(p => !been.contains(p)).foreach(s => step(thisStep, s, distance+1, been :+ thisStep))
          case _ =>
        }
    }
  }

  def isWall(c: Coordinate): Boolean = {
    val sum = c.x*c.x + 3*c.x + 2*c.x*c.y +c.y + c.y*c.y + 1352
    val numOnes = sum.toBinaryString.count(_ == '1')
    numOnes % 2 != 0
  }

  def isDestination(c: Coordinate): Boolean = c.x == 31 && c.y == 39

  def nextSteps(c: Coordinate, from: Coordinate): List[Coordinate] = {
    val possibleDestinations = Set(Coordinate(c.x-1, c.y), Coordinate(c.x+1, c.y),
      Coordinate(c.x, c.y-1), Coordinate(c.x, c.y+1)) - from
    possibleDestinations.filter(p => !isWall(p))
      .filter(p => p.x >= 0 && p.y >= 0)
      .toList
  }
}
