package day4

import scala.io.Source

class Room(spec: String) {
  private val pattern = """([\d]+)\[(.*)\]$""".r
  val (sectorId, checksum) = spec.split('-').last match {
    case pattern(s, c) => (s.toInt, c)
  }
  private val characters = spec.split('-').dropRight(1).mkString("-")

  def isValidRoom: Boolean = {
    val frequencyMap = characters.groupBy(_.toChar).map { p => (p._1, p._2.length) }
    generateChecksum(frequencyMap - '-') == checksum
  }

  private def generateChecksum(frequencyMap: Map[Char, Int]): String = {
    //sort by value descending, then key ascending
    val sortedFrequencyMap = frequencyMap.toList.sortBy({case (k, v) => (-v, k)})
    sortedFrequencyMap.map(_._1).mkString.substring(0, 5)
  }

  def decrypt: String = {
    "id: " + sectorId + ", " + characters.map(translateChar).mkString
  }

  private def translateChar(c: Char): Char = {
    c match {
      case '-' => ' '
      case x => ('a' to 'z').charAt(getNewIndex(x))
    }
  }

  private def getNewIndex(c: Char): Int = {
    val startingIdx = ('a' to 'z').indexOf(c)
    val newIdx = startingIdx + (sectorId % 26)
    newIdx match {
      case v if v > 25 => v - 26
      case v => v
    }
  }
}

object Day4 {
  def main(args: Array[String]): Unit = {
    val rooms = Source.fromFile("inputs/day4.txt").getLines.toList.map(new Room(_))
    val validRooms = rooms.filter(_.isValidRoom)
    val validSectorIds: List[Int] = validRooms.map(_.sectorId)
    println(validSectorIds.sum)
    println(validRooms.map(_.decrypt).filter(p => p contains "north"))
  }
}
