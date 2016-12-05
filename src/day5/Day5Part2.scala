package day5

import java.security.MessageDigest
import scala.collection.immutable.TreeMap

object Day5Part2 {
  val md5 = MessageDigest.getInstance("MD5")

  def main(args: Array[String]): Unit = {
    println(TreeMap(iterateDoorIds(Map.empty, 0).toSeq:_*).values.mkString)
  }

  def iterateDoorIds(password: Map[Int, Char], index: Int): Map[Int, Char] = {
    password match {
      case x if x.size == 8 => password
      case _ => iterateDoorIds(getNextPasswordChar(password, index), index+1)
    }
  }

  def getNextPasswordChar(password: Map[Int, Char], index: Int): Map[Int, Char] = {
    val hash = md5.digest(("cxdnnyjw" + index.toString).getBytes).map(0xFF & _).map{"%02x".format(_)}.foldLeft(""){_+_}
    hash.toList match {
      case x if x.slice(0, 5).mkString == "00000" => validatePasswordChar(password, x)
      case _ => password
    }
  }

  def validatePasswordChar(password: Map[Int, Char], hash: List[Char]): Map[Int, Char] = {
    val position = if (Character.isDigit(hash(5))) hash(5).toString.toInt else 255
    val value = hash(6)
    if (position < 8 && !password.contains(position)) {
      println(s"Found password letter: $value at position $position")
      password + (position -> value)
    }else password
  }
}