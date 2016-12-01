import scala.io.Source
import scala.math.{abs, Pi, cos, sin}

case class Pair(x: Int, y: Int)
case class Position(coord: Pair, heading: Pair)
case class Step(direction: String, distance: Int){
  def this(step: String) = {
    this(step.head.toString, step.tail.toInt)
  }
}

object day1 {
  def main(args: Array[String]): Unit = {
    val steps = Source.fromFile("src/inputs/day1.txt").getLines.toList.head.split(", ").toList.map(new Step(_))
    val finalPosition = takeSteps(Position(Pair(0, 0), Pair(0, 1)), steps)
    println(abs(finalPosition.coord.x) + abs(finalPosition.coord.y))
  }

  def takeSteps(position: Position, steps: List[Step]): Position = {
    steps match {
      case Nil => position
      case step :: tail => takeSteps(newPosition(position, step), tail) 
    } 
  }

  def newPosition(position: Position, step: Step): Position = {
    val newHeading = step.direction match {
      case "L" => turn(Pi/2, position.heading)
      case "R" => turn(-Pi/2, position.heading)
      case _ => position.heading
    }    
    move(step.distance, position.coord, newHeading)
  }

  def turn(angle: Double, heading: Pair): Pair = {
    val newX = heading.x * cos(angle) - heading.y * sin(angle)
    val newY = heading.x * sin(angle) + heading.y * cos(angle)
    Pair(newX.toInt, newY.toInt)
  }

  def move(distance: Int, coord: Pair, heading: Pair): Position = {
    val newX: Int = coord.x + (heading.x * distance)
    val newY: Int = coord.y + (heading.y * distance)
    Position(Pair(newX, newY), heading)
  }
}
