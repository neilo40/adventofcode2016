package day6

import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val rowSpecs = Source.fromFile("inputs/day6.txt").getLines.toList.map(_.toList)
    val colSpecs = transpose(rowSpecs)
    val message = colSpecs.map( x => x.groupBy(_.toChar)
      .map { p => (p._1, p._2.length) }
      .toList.sortBy({case (k, v) => (-v, k)})
      .last._1)
    println(message.mkString)
  }

  def transpose(rows: List[List[Char]]): List[List[Char]] = {
    rows.filter(_.nonEmpty) match {
      case Nil => Nil
      case x: List[List[Char]] => x.map(_.head) :: transpose(x.map(_.tail))
    }
  }
}
