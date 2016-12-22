package day21

import scala.io.Source

object Day21 {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("inputs/day21.txt").getLines.toList.reverse
    println(processInstructions("gbhafcde", instructions))
  }

  def processInstructions(password: String, instructions: List[String]): String = {
    instructions match {
      case Nil => password
      case instruction :: tail => processInstructions(transformPassword(password, instruction), tail)
    }
  }

  def transformPassword(password: String, instruction: String): String = {
    println(s"$instruction on $password")
    val iComponents = instruction.split(' ').toList
    iComponents.head match {
      case "swap" => swap(password, iComponents.tail)
      case "reverse" => reverse(password, iComponents.tail)
      case "rotate" => rotate(password, iComponents.tail)
      case "move" => move(password, iComponents.tail)
    }
  }

  def swap(password: String, iComponents: List[String]): String = {
    iComponents.head match {
      case "position" => swapPosition(password, iComponents(1).toInt, iComponents(4).toInt)
      case "letter" => swapLetter(password, iComponents(1).head, iComponents(4).head)
    }
  }

  def swapPosition(password: String, a: Int, b: Int): String = {
    swapLetter(password, password(a), password(b))
  }

  def swapLetter(password: String, first: Char, second: Char): String = {
    password.replace(first, 'z')
      .replace(second, first)
      .replace('z', second)
  }

  def reverse(password: String, iComponents: List[String]): String = {
    val start = iComponents(1).toInt
    val end = iComponents(3).toInt + 1
    val reversedPart = password.substring(start, end).reverse
    password.take(start) + reversedPart + password.takeRight(password.length - end)
  }

  def rotate(password: String, iComponents: List[String]): String = {
    iComponents.head match {
      case "based" =>
        val amount = password.indexOf(iComponents.last)
        val extraAmount = if (amount >= 4) 1 else 0
        rotateByAmount(password, "right", 1 + amount + extraAmount)  // Need to rotate until letter is in position == rotations done
      case _ => rotateByAmount(password, iComponents.head, iComponents(1).toInt)
    }
  }

  def rotateByAmount(password: String, direction: String, _amount: Int): String = {
    val amount = if (_amount > password.length) _amount - password.length else _amount
    direction match {
      case "left" => password.takeRight(amount) + password.dropRight(amount)
      case "right" => password.drop(amount) + password.take(amount)
    }
  }

  def move(password: String, iComponents: List[String]): String = {
    val fromIndex = iComponents(4).toInt
    val toIndex = iComponents(1).toInt

    val charToMove = password.charAt(fromIndex).toString
    val sb = new StringBuilder(password)
    val intermediatePassword = sb.deleteCharAt(fromIndex).toString
    val (start, end) = intermediatePassword.splitAt(toIndex)
    start + charToMove + end
  }
}
