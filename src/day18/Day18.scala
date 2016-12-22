package day18

object Day18 {
  def main(args: Array[String]): Unit = {
    val firstRow = "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."
    val safeTiles = calcRows(List(firstRow)).map(r => r.count(_ == '.')).sum
    println(safeTiles)
  }

  def calcRows(previousRows: List[String]): List[String] = {
    previousRows.length match {
      case 400000 => previousRows
      case _ =>
        val triples = ('.' + previousRows.last + '.').sliding(3).toList
        calcRows(previousRows :+ calcNewRow(triples, ""))
    }
  }

  def calcNewRow(triples: List[String], row: String): String = {
    triples match {
      case Nil => row
      case triple :: tail => calcNewRow(tail, row + calcNewTile(triple))
    }
  }

  def calcNewTile(previousTiles: String): Char = {
    previousTiles match {
      case "^.." | "..^" | "^^." | ".^^" => '^'
      case _ => '.'
    }
  }
}
