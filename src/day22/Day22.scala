package day22

import scala.io.Source

class Node(_spec: String){
  private val spec = _spec.split("\\s+")
  val nodeName = spec(0).split('/').last
  val size = spec(1).dropRight(1).toInt
  val used = spec(2).dropRight(1).toInt
  val avail = spec(3).dropRight(1).toInt
}

object Day22 {
  def main(args: Array[String]): Unit = {
    val allNodes = Source.fromFile("inputs/day22.txt").getLines.map(new Node(_)).toList
    val viableANodes = allNodes.filter(_.used != 0)
    println(viableANodes.map(numViableBNodesForA(_, allNodes)).sum)
  }

  def numViableBNodesForA(a: Node, nodes: List[Node]): Int = {
    nodes.count(b => b != a && a.used <= b.avail)
  }
}
