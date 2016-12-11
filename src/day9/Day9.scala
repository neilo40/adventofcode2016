package day9

import scala.io.Source

case class State(decompressed: String, compressed: String)
class Instruction(instruction: String){
  val depth = instruction.split('x').head.toInt
  val repeat = instruction.split('x').last.toInt
}

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("inputs/day9.txt").getLines.toList.head
    val startingState = State("", input)
    println(iterateInput(startingState).length)
  }

  def iterateInput(state: State): String = {
    state.compressed match {
      case "" => state.decompressed
      case _ => iterateInput(decompress(state))
    }
  }

  def decompress(state: State): State = {
    state.compressed.head match {
      case '(' =>
        val (instructionString, body) = state.compressed.tail.span(_ != ')')
        extractSequence(new Instruction(instructionString), State(state.decompressed, body.tail))
      case _ => State(state.decompressed + state.compressed.head, state.compressed.tail)
    }
  }

  def extractSequence(instruction: Instruction, state: State): State = {
    val remainingCompressed = state.compressed.drop(instruction.depth)
    val newDecompressed = state.decompressed + state.compressed.take(instruction.depth) * instruction.repeat
    State(newDecompressed, remainingCompressed)
  }
}
