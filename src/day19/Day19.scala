package day19

object Day19 {
  def main(args: Array[String]): Unit = {
    val elves = 1 to 3001330
    println(steal(elves.toList).head)
  }

  def steal(elves: List[Int]): List[Int] = {
    elves.length match {
      case 1 => elves
      case _ =>
        steal(remainingElves(elves))
    }
  }

  def remainingElves(elves: List[Int]): List[Int] = {
    elves.length % 2 match {
      case 0 => removeElves(elves)
      case _ => List(elves.last) ++ removeElves(elves.dropRight(1))
    }
  }

  def removeElves(elves: List[Int]): List[Int] = {
    elves.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
  }
}
