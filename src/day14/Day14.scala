package day14

import java.security.MessageDigest

class Hash(_index: Int, md5: MessageDigest){
  private val salt = "ngcjuoqr"
  val index = _index
  val hash = get2016thHash(0, genHash(salt + index.toString))
  var tripleString = ' '

  def get2016thHash(i: Int, prevHash: String): String = {
    i match {
      case 2016 => prevHash
      case _ => get2016thHash(i + 1, genHash(prevHash))
    }
  }

  def genHash(h: String): String = {
    md5.digest(h.getBytes).map(0xFF & _).map{"%02x".format(_)}.foldLeft(""){_+_}
  }

  def hasTriple: Boolean = {
    val repeat = hash.sliding(3).filter(p => p.toSet.size == 1).toList.headOption
    repeat match {
      case Some(x) =>
        tripleString = x.head
        true
      case None => false
    }
  }

  def hasQuin(character: Char): Boolean = {
    hash.sliding(5).exists(p => p.toSet.size == 1 && p.toSet.contains(character))
  }
}

object Day14 {
  val md5 = MessageDigest.getInstance("MD5")

  def main(args: Array[String]): Unit = {
    println(find64Hashes(0, 0))
  }

  def find64Hashes(index: Int, _found: Int): Int = {
    val found = if (isKey(new Hash(index, md5))) _found + 1 else _found
    found match {
      case 64 => index
      case _ => find64Hashes(index + 1, found)
    }
  }

  def isKey(hash: Hash): Boolean = {
    hash.hasTriple && nextThousandHashes(hash.index).exists(_.hasQuin(hash.tripleString))
  }

  def nextThousandHashes(index: Int): IndexedSeq[Hash] = {
    (1 to 1000).map(i => new Hash(index + i, md5))
  }
}
