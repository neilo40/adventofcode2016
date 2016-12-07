package day7

import scala.io.Source

object Day7Part2 {
  def main(args: Array[String]): Unit = {
    val addresses = Source.fromFile("inputs/day7.txt").getLines.toList
    println(addresses.count(supportsSsl))
  }

  def supportsSsl(s: String): Boolean = {
    val supernetAba = getSupernet(s).map(getAba).foldLeft(List[String]())((a, b) => a ::: b)
    val hypernetBab = getHypernet(s).map(getAba).foldLeft(List[String]())((a, b) => a ::: b)
    for (aba <- supernetAba){
      val bab = aba(1).toString + aba(0).toString + aba(1).toString
      if (hypernetBab contains bab) return true
    }
    false
  }

  def getHypernet(s: String): List[String] = {
    val regex = "\\[(.*?)\\]".r
    val matches = regex.findAllIn(s).matchData.map(m => m.group(1)).toList
    matches
  }

  def getSupernet(s: String): List[String] = {
    s.replaceAll("\\[.*?\\]", ",").split(",").toList
  }

  def getAba(s: String): List[String] = {
    s.sliding(3).filter(p => p(0) == p(2) && p(0) != p(1)).toList
  }
}