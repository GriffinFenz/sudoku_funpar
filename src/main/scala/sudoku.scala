import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.parallel.CollectionConverters.*

object sudoku {

  // Adding the input into the board
  def parse_input(input: String, board: SudokuBoard.Board) = {
    List.tabulate(81)(_ + 0).foreach(index => {
      val data: String = if input(index) == '.' then "123456789" else input(index).toString
      board.squares(index).addData(data)
    })
  }

  // If a number found in a square then eliminate that number from all it's peers
  def eliminatePeers(board: SudokuBoard.Board) = {
    board.squares.foreach(square => {
      val size = square.getData.length
      if size == 1 then square.getPeers.foreach(peer => peer.removeData(square.getData))
    })
  }

  // If a unit has a unique value then map it to the square that contains that unique value
  def eliminateAllUnit(units: SudokuBoard.Board) = {
    def eliminateOneUnit(unit: Vector[SudokuBoard.Square]) = {
      val countMap: mutable.HashMap[Char, Int] = new mutable.HashMap()
      unit.foreach(square => square.getData.foreach(char => countMap.update(char, countMap.getOrElse(char, 0)+1)))
      val countMap2: mutable.HashMap[Char, Int] = countMap.filter((_, int) => int==1)
      unit.foreach(square => countMap2.keys.foreach(char => if square.getData.contains(char) then square.addData(char.toString)))
    }
    units.box_group.foreach(unit => eliminateOneUnit(unit))
    units.row_group.foreach(unit => eliminateOneUnit(unit))
    units.col_group.foreach(unit => eliminateOneUnit(unit))
  }

  // Does Constraint Propagation no.1
  def eliminateAll(board: SudokuBoard.Board) = {
    var beforeEliminate: String = ""
    var afterEliminate: String = ""
    board.squares.map(square => square.getData).foreach(data => afterEliminate+=data)
    while (beforeEliminate != afterEliminate) {
      eliminatePeers(board)
      eliminateAllUnit(board)
      beforeEliminate = afterEliminate
      afterEliminate = ""
      board.squares.map(square => square.getData).foreach(data => afterEliminate+=data)
    }
  }


  def main(args: Array[String]): Unit = {
    val sudoku_board: SudokuBoard.Board = new SudokuBoard.Board
    //val input_board: String = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
    val input_board: String = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
    parse_input(input_board, sudoku_board)
    println(sudoku_board.squares)
    eliminateAll(sudoku_board)
    println(sudoku_board.squares)


    //println("123456789".contains("1"))
    //println("123456789".length)
    //println("123456789".slice(0, 2) + "123456789".slice(3, 9)) // If I found 3
  }
}

// TODO: Add Cases for impossible boards so it can break instead of finding a solution forever
