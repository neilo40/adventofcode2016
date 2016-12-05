package day5

import java.security.MessageDigest

object Day5 {
  val md5 = MessageDigest.getInstance("MD5")

  def main(args: Array[String]): Unit = {
    println(iterateDoorIds(List.empty, 0).mkString)
  }

  def iterateDoorIds(password: List[Char], index: Int): List[Char] = {
    password match {
      case x if x.length == 8 => password
      case _ => iterateDoorIds(getNextPasswordChar(password, index), index+1)
    }
  }

  def getNextPasswordChar(password: List[Char], index: Int): List[Char] = {
    val hash = md5.digest(("cxdnnyjw" + index.toString).getBytes).map(0xFF & _).map{"%02x".format(_)}.foldLeft(""){_+_}
    hash.toList match {
      case x if x.slice(0, 5).mkString == "00000" => {
        println("Found password letter " + x(5) + s" at index $index")
        password :+ x(5)
      }
      case _ => password
    }
  }
}
