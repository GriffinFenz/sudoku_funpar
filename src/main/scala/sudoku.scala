import SudokuBoard.*
import scala.util.control.Breaks._
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.parallel.CollectionConverters.*

object sudoku {

  // Adding the input into the board
  def parse_input(input: String, board: SudokuBoard.Board) = {
    List.tabulate(81)(_ + 0).foreach(index => {
      val data: String = if input(index) == '.' then "123456789" else input(index).toString
      board.squares(index).setData(data)
    })
  }

  def print_board(board: Board) = {
    List.tabulate(81)(_ + 0).foreach(index => {
      if index % 27 == 0 then println()
      if index % 9 == 0 then  println() else
      if index % 3 == 0 then print(". ")
      print(board.squares(index).getData)
      print(" ")
    })
  }

  // If a number found in a square then eliminate that number from all it's peers
  def eliminatePeers(board: SudokuBoard.Board): Boolean = {
    var clean_run = true
    board.squares.foreach(square => {
      val size = square.getData.length
      if size == 1 then square.getPeers.foreach(peer =>
        if peer.getData.contains(square.getData) && peer.getData.length == 1
        then clean_run = false else peer.removeData(square.getData))
    })
    clean_run
  }

  // If a unit has a unique value then map it to the square that contains that unique value
  // TODO:: FIX THIS GARBAGE (I think I fixed it)
  def eliminateAllUnit(units: SudokuBoard.Board): Boolean = {
    var clean_run = false
    def eliminateOneUnit(unit: Vector[SudokuBoard.Square]) = {
      val countMap: mutable.HashMap[Char, Int] = new mutable.HashMap()
      unit.foreach(square => square.getData.foreach(char => countMap.update(char, countMap.getOrElse(char, 0)+1)))
      val countMap2: mutable.HashMap[Char, Int] = countMap.filter((_, int) => int==1)
      unit.foreach(square => countMap2.keys.foreach(char => if square.getData.contains(char)
      then {if square.getData != char.toString then clean_run = true; square.setData(char.toString) }))
    }
    units.box_group.foreach(unit => eliminateOneUnit(unit))
    units.row_group.foreach(unit => eliminateOneUnit(unit))
    units.col_group.foreach(unit => eliminateOneUnit(unit))
    clean_run
  }

  // Does Constraint Propagation no.1
  def eliminateAll(board: SudokuBoard.Board): Boolean = {
    var clean_run: Boolean = true
    var beforeEliminate: String = ""
    var afterEliminate: String = ""
    board.squares.map(square => square.getData).foreach(data => afterEliminate+=data)
    while (beforeEliminate != afterEliminate) {
      if eliminatePeers(board) || eliminateAllUnit(board) then clean_run = true else clean_run = false
      beforeEliminate = afterEliminate
      afterEliminate = ""
      board.squares.map(square => square.getData).foreach(data => afterEliminate+=data)
    }

    clean_run
  }

  def solve_board(board: SudokuBoard.Board) = {

    def is_solved(board: SudokuBoard.Board): Boolean = {
      board.squares.count(square => square.getData.length == 1) == 81
    }

    def try_square(square: Square, saved_values: mutable.HashMap[Square, String]): Boolean = {
      println(saved_values)
      var clean_run = false
      square.getData.foreach(x => {
        // If the value worked then do nothing else reset it back to saved point and try next value
        if clean_run then () else { saved_values.foreach(item => item._1.setData(item._2))
        square.setData(x.toString)
        //println(s"${square.getName}: ${square.getData}")
        clean_run = eliminateAll(board) }
        //println(s"Clean?: ${clean_run}, on value ${x}")
      })
      // TODO:: Might need to remove this if a value must work no matter what
      if !clean_run then saved_values.foreach(item => item._1.setData(item._2))
      clean_run
    }

    def try_all_squares(filtered: Vector[Square], saved_value: mutable.HashMap[Square, String], index: Int): Boolean = {
      if is_solved(board) || index >= filtered.size then true else {
        if try_square(filtered(index), saved_value) then {
          val filtered_by_lowest = board.squares.filter(_.getData.length > 1).sortBy(_.getData.length)
          val saved_values_new: mutable.HashMap[Square, String] = new mutable.HashMap[Square, String]()
          filtered_by_lowest.foreach(square => saved_values_new.put(square, square.getData))
          try_all_squares(filtered_by_lowest, saved_values_new, 0)
        }
        else {
          try_all_squares(filtered, saved_value, index + 1)
        }
        false
      }
    }

    var a = 0;

    //eliminateAll(board)
    // TODO:: Change Condition to until board is solved
    for (a <- 1 to 1) {
      // Saving the values that we will change by trying out brute force method
      val filtered_by_lowest = board.squares.filter(_.getData.length > 1).sortBy(_.getData.length)
      val saved_values: mutable.HashMap[Square, String] = new mutable.HashMap[Square, String]()
      filtered_by_lowest.foreach(square => saved_values.put(square, square.getData))
      // Trying one square at a time
      // TODO:: If one square works I need to loop again and filter by lowest again and saved_values again
      // TODO:: If one square fails then yea I need this thing
      //filtered_by_lowest.foreach(square => try_square(square, saved_values))

      try_all_squares(filtered_by_lowest, saved_values, 0)
    }

  }

  // TODO:: Broke
  def truly_solved(board: Board): Boolean = {
    val a: Int = board.col_group.count(square_vector => square_vector.distinct.size == 9)
    val b: Int = board.row_group.count(square_vector => square_vector.distinct.size == 9)
    val c: Int = board.box_group.count(square_vector => square_vector.distinct.size == 9)
    a == 9 && b == 9 && c == 9
  }

  def main(args: Array[String]): Unit = {
    val sudoku_board: SudokuBoard.Board = new SudokuBoard.Board
    val input_board1: String = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
    val input_board2: String = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
    val input_board3: String = "............2.6.9.....1.627.61.842.9..759...442..6378..38657.1264.3.1978172.4..5."
    val input_board4: String = ".53..1.8.....8.25..1.57...46..43.......69.5.....1.5..8.987.2...2.7.....95....982."
    val input_board5: String = "95324178647698325181257693468543719212469857373912546839871264526785431954136982."
    parse_input(input_board3, sudoku_board)
    //sudoku_board.squares(80).setData("7")
    println(sudoku_board.squares)
    solve_board(sudoku_board)
    print_board(sudoku_board)
    println()
    println(truly_solved(sudoku_board))



    //println("123456789".contains("1"))
    //println("123456789".length)
    //println("123456789".slice(0, 2) + "123456789".slice(3, 9)) // If I found 3
  }
}

// TODO: Add Cases for impossible boards so it can break instead of finding a solution forever
// TODO: Test with things that wont pass first value tried in first block
// TODO: Try where first block fully fails
// TODO: Try where first block passes but the code isnt fixed yet
// TODO: Fix my loopy loop
// TODO: Change code design , why it so ugly
