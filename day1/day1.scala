import scala.io.Source
import scala.math.{abs, Pi, cos, sin}

case class Coord(x: Int, y: Int)
case class Position(coord: Coord, heading: Coord)
case class Step(direction: String, distance: Int) = {
  def this(step: String) = {
    this(step.substring(0, 1), step.substring(1, 2).toInt)
  }
}

object day1 {
  def main(args: Array[String]): Unit = {
    val steps = Source.fromFile("inputs/day1.txt").getLines.toList.head.split(", ").toList.map(Step)
    val finalPosition = takeSteps(Position(Coord(0, 0), Coord(0, 0)), steps)
    println(abs(finalPosition.x) + abs(finalPosition.y))
  }

  def takeSteps(position: Position, steps: List[Step]): Position = {
    steps.match {
      case Nil => position
      case step :: tail => takeSteps(newPosition(position, step), tail) 
    } 
  }

  def newPosition(position: Position, step: Step): Position) = {
    val newHeading = step.direction match {
      case "R" => turn(Pi/2, position.heading)
      case "L" => turn(-Pi/2, position.heading)
      case _ => position.heading
    }    
    move(step.distance, position.coord, newHeading)
  }

  def turn(angle: Double, heading: Coord): Coord = {
    val newX = heading.x * cos(angle) - heading.y * sin(angle)
    val newY = heading.x * sin(angle) + heading.y * cos(angle)
    Coord(newX, newY)
  }

  def move(distance: Int, position: Coord, heading: Coord): Coord = {
    position
  }
}
