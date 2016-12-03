package day2

import scala.collection.immutable.HashMap
import scala.io.Source

class KeypadDigit(var value: Char) {
  var left: KeypadDigit = this
  var right: KeypadDigit = this
  var up: KeypadDigit = this
  var down: KeypadDigit = this

  def digitAtDirection(direction: Char): KeypadDigit = {
    direction match {
      case 'U' => up
      case 'D' => down
      case 'L' => left
      case 'R' => right
      case _ => this
    }
  }
}

object Day2Part2 {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("inputs/day2.txt").getLines.toList
    val digits = setupDigits()
    var currentDigit = digits('5')
    for (instruction <- instructions) {
      currentDigit = processMoves(currentDigit, instruction.toList)
      println(currentDigit.value)
    }
  }

  def processMoves(digit: KeypadDigit, moves: List[Char]): KeypadDigit = {
    moves match {
      case Nil => digit
      case direction :: tail => processMoves(digit.digitAtDirection(direction), tail)
    }
  }

  def setupDigits(): HashMap[Char, KeypadDigit] = {
    var digits = HashMap.empty[Char, KeypadDigit]
    for (i <- "123456789ABCD".toList) digits += (i -> new KeypadDigit(i))

    digits('1').down = digits('3')
    digits('1').left = digits('1')

    digits('2').right = digits('3')
    digits('2').down = digits('6')

    digits('3').up = digits('1')
    digits('3').right = digits('4')
    digits('3').down = digits('7')
    digits('3').left = digits('2')

    digits('4').down = digits('8')
    digits('4').left = digits('3')

    digits('5').right = digits('6')

    digits('6').up = digits('2')
    digits('6').right = digits('7')
    digits('6').down = digits('A')
    digits('6').left = digits('5')

    digits('7').up = digits('3')
    digits('7').right = digits('8')
    digits('7').down = digits('B')
    digits('7').left = digits('6')

    digits('8').up = digits('4')
    digits('8').right = digits('9')
    digits('8').down = digits('C')
    digits('8').left = digits('7')

    digits('9').left = digits('8')

    digits('A').up = digits('6')
    digits('A').right = digits('B')

    digits('B').up = digits('7')
    digits('B').right = digits('C')
    digits('B').down = digits('D')
    digits('B').left = digits('A')

    digits('C').up = digits('8')
    digits('C').left = digits('B')

    digits('D').up = digits('B')

    digits
  }
}
