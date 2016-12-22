package day20

import scala.io.Source

class IpRange(_spec: String){
  private val parts = _spec.split('-').map(_.toLong)
  val from = parts(0)
  val to = parts(1)

  def containedBy(that: IpRange): Boolean = {
    from >= that.from && to <= that.to
  }

  def overlaps(that: IpRange): Boolean = {
    from <= that.to + 1 && to > that.to
  }

  def separateFrom(that: IpRange): Boolean = {
    from > that.to
  }

  override def toString = s"from: $from, to: $to"
}

object Day20 {
  def main(args: Array[String]): Unit = {
    val blackListedIps = Source.fromFile("inputs/day20.txt").getLines
      .map(new IpRange(_))
      .toList.sortWith(_.from < _.from)
    println(processRanges(blackListedIps.tail, blackListedIps.head).to + 1)
  }

  def processRanges(ranges: List[IpRange], thisRange: IpRange): IpRange = {
    ranges.head match {
      case r if r.containedBy(thisRange) => processRanges(ranges.tail, thisRange)
      case r if r.overlaps(thisRange) => processRanges(ranges.tail, r)
      case r if r.separateFrom(thisRange) => thisRange
    }
  }
}
