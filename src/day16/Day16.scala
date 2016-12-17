package day16

object Day16{
  val dataSize = 272

  def main(args: Array[String]): Unit = {
    val input = "01111001100111011"
    val data = genData(input).take(dataSize)
    println(genChecksum(data))
  }

  def genData(a: String): String = {
    a.length match {
      case x if (x >=  dataSize) => a
      case _ => genData(genNewA(a))
    }
  }

  def genNewA(a: String): String = a + '0' + genB(a)

  def genB(a: String): String = {
    a.reverse
      .replace('0', 'a')
      .replace('1', '0')
      .replace('a', '1')
  }

  def genChecksum(data: String): String = {
    val sum = genCandidateChecksum(data)
    sum.length % 2 match {
      case 0 => genChecksum(genCandidateChecksum(sum))
      case _ => sum
    }
  }

  def genCandidateChecksum(data: String): String = {
    data.sliding(2, 2).toList.map(condensePair).mkString
  }

  def condensePair(p: String): Char = {
    p match {
      case x if (x.head == x.last) => '1'
      case _ => '0'
    }
  }
}
