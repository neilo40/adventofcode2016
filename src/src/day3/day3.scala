import scala.io.Source

object day3 {
  def main(args: Array[String]): Unit = {
    val triangleSpecs = Source.fromFile("src/inputs/day3.txt").getLines.toList
    val validTriangles = triangleSpecs.filter(isValidTriangle)
    println(validTriangles.length)
  }

  def isValidTriangle(spec: String): Boolean = {
    val sides = spec.split("\\s+").toList.tail.map(_.toInt).sortWith(_ > _)
    sides.head < sides.tail.sum
  }
}
