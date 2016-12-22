package day15

import scala.io.Source

class Disc(_spec: String){
  private val spec = _spec.split(' ')
  val id = spec(1)
  val positions = spec(3).toInt
  val startingPos = spec.last.stripSuffix(".").toInt

  def willDrop(time: Int): Boolean = (startingPos + time) % positions == 0
}

object Day15 {
  def main(args: Array[String]): Unit = {
    val discs = Source.fromFile("inputs/day15.txt").getLines.map(new Disc(_)).toList
    checkIfDrops(0, discs :+ new Disc("Disc #7 has 11 positions; at time=0, it is at position 0."))
  }

  def checkIfDrops(time: Int, discs: List[Disc]): Boolean = {
    dropsFully(time, discs) match {
      case false => checkIfDrops(time + 1, discs)
      case _ =>
        println(time)
        true
    }
  }

  def dropsFully(time: Int, discs: List[Disc]): Boolean = {
    discs.head.willDrop(time + 1) match {
      case true => if (discs.tail.isEmpty) true else dropsFully(time + 1, discs.tail)
      case _ => false
    }
  }
}
