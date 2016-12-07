package day7

import scala.io.Source

object Day7 {
  def main(args: Array[String]): Unit = {
    val addresses = Source.fromFile("inputs/day7.txt").getLines.toList
    println(addresses.count(supportsTls))
  }

  def supportsTls(s: String): Boolean = {
    val hyperNetHasAbba = getHypernet(s).map(hasAbba).reduce(_ || _)
    val supernetHasAbba = getSupernet(s).map(hasAbba).reduce(_ || _)
    supernetHasAbba && !hyperNetHasAbba
  }

  def getHypernet(s: String): List[String] = {
    val regex = "\\[(.*?)\\]".r
    val matches = regex.findAllIn(s).matchData.map(m => m.group(1)).toList
    matches
  }

  def getSupernet(s: String): List[String] = {
    s.replaceAll("\\[.*?\\]", ",").split(",").toList
  }

  def hasAbba(s: String): Boolean = {
    val characterPairs = s.sliding(2).toList
    val triplets = characterPairs.sliding(3).toList
    val filteredTriplets = triplets.filter(p => p.head != p(1))
    for (characterPair <- characterPairs) {
      if (filteredTriplets.contains(generateAbbaList(characterPair))) return true
    }
    false
  }

  def generateAbbaList(s: String): List[String] = {
    List(s, s(1).toString + s(1).toString, s(1).toString + s(0).toString)
  }
}
