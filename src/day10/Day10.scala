package day10

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Receiver(){
  var chips = new ListBuffer[Int]()
  def receiveChip(chip: Int): Unit = {
    chips += chip
  }
}
class Output(id: Int) extends Receiver{}
class Bot(_id: Int) extends Receiver{
  val id = _id
  var low: Receiver = _
  var high: Receiver = _

  def tick(): Boolean = {
    if (chips.contains(61) && chips.contains(17)) {
      println(id)
    }
    if (chips.length == 2) {
      val sortedChips = chips.sorted
      low.receiveChip(sortedChips.head)
      high.receiveChip(sortedChips.last)
      chips.clear()
      return true
    }
    false
  }
}

object Day10 {
  def main(args: Array[String]): Unit = {
    val bots = (0 to 300).map(new Bot(_)).toList
    val outputs = (0 to 30).map(new Output(_)).toList
    val definitions: List[String] = Source.fromFile("inputs/day10.txt").getLines.toList
    definitions.filter(d => d.startsWith("value")).foreach(assignValueToBot(_, bots))
    definitions.filter(d => d.startsWith("bot")).foreach(connectBots(_, bots, outputs))
    while (bots.exists(_.tick())){}
    println(outputs.head.chips.head * outputs(1).chips.head * outputs(2).chips.head)
  }

  def connectBots(_definition: String, listOfBots: List[Bot], listOfOutputs: List[Output]): Unit = {
    val definition = _definition.split(" ")
    val thisBot = listOfBots(definition(1).toInt)
    val lowTarget = createReceiver(listOfBots, listOfOutputs, definition(5), definition(6).toInt)
    val highTarget = createReceiver(listOfBots, listOfOutputs, definition(10), definition(11).toInt)
    thisBot.low = lowTarget
    thisBot.high = highTarget
  }

  def createReceiver(listOfBots: List[Bot], listOfOutputs: List[Output], kind: String, id: Int): Receiver = {
    kind match {
      case "output" => listOfOutputs(id)
      case "bot" => listOfBots(id)
    }
  }

  def assignValueToBot(_definition: String, listOfBots: List[Bot]): Unit = {
    val definition = _definition.split(" ")
    val bot = listOfBots(definition(5).toInt)
    bot.chips += definition(1).toInt
  }
}
