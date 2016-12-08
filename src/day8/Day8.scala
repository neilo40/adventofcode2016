package day8

import scala.io.Source

class Display(_matrix: Option[Array[Array[Boolean]]] = None){
  val matrixRows = 6
  val matrixCols = 50
  val matrix = _matrix.getOrElse(Array.ofDim[Boolean](matrixRows, matrixCols))

  def setRect(width: Int, height: Int): Display = {
    val newMatrix = (0 until matrixRows).map(row => setRow(height, row, width)).toArray
    new Display(Some(newMatrix))
  }

  private def setRow(height: Int, row: Int, width: Int): Array[Boolean] ={
    row match {
      case x if row < height => Array.fill(width)(true) ++ matrix(row).drop(width)
      case _ => matrix(row)
    }
  }

  def rotateRows(rowIndex: Int, amount: Int): Display = {
    val newMatrix = (0 until matrixRows).map(row => rotateRow(row, rowIndex, amount)).toArray
    new Display(Some(newMatrix))
  }

  private def rotateRow(row: Int, rowToRotate: Int, amount: Int): Array[Boolean] = {
    row match {
      case x if x == rowToRotate => matrix(row).takeRight(amount) ++ matrix(row).dropRight(amount)
      case _ => matrix(row)
    }
  }

  def rotateCols(colIndex: Int, amount: Int): Display = {
    val newMatrix = (0 until matrixRows).map(row => rotateCol(row, colIndex, amount)).toArray
    new Display(Some(newMatrix))
  }

  private def rotateCol(row: Int, colToRotate: Int, amount: Int): Array[Boolean] = {
    val newRowIndex = row - amount
    val correctedNewRowIndex = if (newRowIndex < 0) newRowIndex + matrixRows else newRowIndex
    val newColValue = matrix(correctedNewRowIndex)(colToRotate)
    matrix(row).take(colToRotate) ++ Array(newColValue) ++ matrix(row).drop(colToRotate+1)
  }

  def count: Int = {
    matrix.map(_.count(_ == true)).sum
  }

  def string: String = {
    matrix.map(_.map(v => if (v) '#' else ' ').mkString).mkString("\n")
  }
}

object Day8 {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("inputs/day8.txt").getLines.toList
    val finalDisplay = processInstructions(new Display(), instructions)
    println(finalDisplay.count)
    println(finalDisplay.string)
  }

  def processInstructions(display: Display, instructions: List[String]): Display = {
    instructions match {
      case Nil => display
      case instruction :: tail => processInstructions(updateDisplay(instruction, display), tail)
    }
  }

  def updateDisplay(instruction: String, display: Display): Display = {
    instruction.split(" ") match {
      case x if x(0) == "rect" =>
        val dimensions = x(1).split('x').map(_.toInt)
        display.setRect(dimensions(0), dimensions(1))
      case x if x(1) == "row" =>
        val rowIndex = x(2).split('=').last.toInt
        display.rotateRows(rowIndex, x(4).toInt)
      case x if x(1) == "column" =>
        val colIndex = x(2).split('=').last.toInt
        display.rotateCols(colIndex, x(4).toInt)
      case _ => display
    }
  }

}
