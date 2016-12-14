package day12

import scala.collection.mutable
import scala.io.Source

object Day12 {
  var registers = mutable.Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)

  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("inputs/day12.txt").getLines.map(_.split(" ").toList).toList
    var pc = 0
    while (pc < instructions.length) pc = execute(instructions(pc), pc)
    println(registers("a"))
  }

  def execute(instruction: List[String], pc: Int): Int = {
    instruction.head match {
      case "cpy" => copy(instruction(1), instruction(2), pc)
      case "jnz" => jump(instruction(1), instruction(2).toInt, pc)
      case "inc" => increment(instruction(1), pc)
      case "dec" => decrement(instruction(1), pc)
    }
  }

  def copy(x: String, y: String, pc: Int): Int = {
    registers(y) = operandValue(x)
    pc + 1
  }

  def jump(x: String, y: Int, pc: Int): Int = {
    if (operandValue(x) != 0) pc + y else pc + 1
  }

  def increment(x: String, pc: Int): Int = {
    registers(x) += 1
    pc + 1
  }

  def decrement(x: String, pc: Int): Int = {
    registers(x) -= 1
    pc + 1
  }

  private def operandValue(v: String): Int = {
    v match {
      case "a" | "b" | "c" | "d" => registers(v)
      case _ => v.toInt
    }
  }
}
