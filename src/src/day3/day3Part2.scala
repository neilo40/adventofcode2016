import scala.io.Source

object day3Part2 {
  def main(args: Array[String]): Unit = {
    val rowSpecs = Source.fromFile("src/inputs/day3.txt").getLines.toList.map(_.split("\\s+").toList.tail)
    val allSides: List[String] = rowSpecs.map(_.head) ::: rowSpecs.map(_(1)) ::: rowSpecs.map(_(2))
    val triangleSpecs = allSides.grouped(3).toList
    val validTriangles = triangleSpecs.filter(isValidTriangle)
    println(validTriangles.length)
  }

  def isValidTriangle(sides: List[String]): Boolean = {
    val sortedSides = sides.map(_.toInt).sortWith(_ > _)
    sortedSides.head < sortedSides.tail.sum
  }
}
