object SudokuBoard {

  class Board {
    private val letters: Vector[String] = Vector("A", "B", "C", "D", "E", "F", "G", "H", "I")
    private val numbers: Vector[String] = Vector("1", "2", "3", "4", "5", "6", "7", "8", "9")
    // Building the board of 81 squares
    val squares: Vector[Square] = letters.flatMap(letter => numbers.map(number => new Square(letter+number)))

    // Making the units for each square
    val row_group: Vector[Vector[Square]] = Vector(0, 9, 18, 27, 36, 45, 54, 63, 72).map(i => squares.slice(i, i+9))
    val col_group: Vector[Vector[Square]] = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8)
      .map(i => Vector(squares(i), squares(i+9), squares(i+18), squares(i+27), squares(i+36),
        squares(i+45), squares(i+54), squares(i+63), squares(i+72)))
    val box_group: Vector[Vector[Square]] = {
      var ans: Vector[Vector[Square]] = Vector()
      var a: Int = 0
      var b: Int = 0
      while (b <= 69) {
        var thing: Vector[Square] = Vector()
        thing = squares.slice(b, b+3):++squares.slice(b+9, b+12):++squares.slice(b+18, b+21)
        ans = ans:+thing
        b = b+3
        if ans.size % 3 == 0 then {a = a+27; b = a}
      }
      ans
    }
    row_group.foreach(group => group.foreach(square => square.addUnits(group)))
    col_group.foreach(group => group.foreach(square => square.addUnits(group)))
    box_group.foreach(group => group.foreach(square => square.addUnits(group)))

    // Making the peers for each square
    squares.foreach(square => square.addPeers(square.getUnits.flatten.filter(square1 => square1.getName!=square.getName).toSet))
    /*
    val list: List[Int] = List.tabulate(81)(_ + 0)
    def parse_input(input: String) = List.tabulate(81)(_ + 0).foreach(index => {
      val data: String = if input(index) == '.' then "123456789" else input(index).toString
      squares(index).addData(data)
    })
    */
  }

  class Square(n: String) {
    private val name: String = n
    private var units: Vector[Vector[Square]] = Vector()
    private var peers: Set[Square] = Set()
    private var data: String = ""

    def addUnits(that: Vector[Square]) = units = units:+that
    def getUnits: Vector[Vector[Square]] = units

    def addPeers(that: Set[Square]) = peers = that
    def getPeers: Set[Square] = peers

    def setData(that: String) = data = that
    def getData: String = data
    def removeData(that: String) = {
      val i: Int = if data.contains(that) then data.indexOf(that) else -1
      if i != -1 then data = data.slice(0, i)+data.slice(i+1, data.length)
    }

    def getName: String = name

    override def toString: String = s"${name}: ${data}"
  }

  def main(args: Array[String]): Unit = {
    val a: Board = new Board()
    println(a.squares)
    println(a.row_group)
    println(a.col_group)
    println(a.box_group)
    //a.squares.foreach(square => println(square.getUnits))
    //println(a.squares(19).getUnits)
    //println(a.squares(19).getPeers)
  }

}
