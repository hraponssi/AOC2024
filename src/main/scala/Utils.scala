import collection.mutable.ArrayBuffer

// General helper class for grid based problems
class Grid[A](val height: Int, val width: Int, defaultValue: A) extends Iterable[(Int, Int)] {
  private var mapping: ArrayBuffer[ArrayBuffer[A]] = ArrayBuffer.fill(height)(ArrayBuffer.fill(width)(defaultValue))

  // Utilities
  def exists(pair: (Int, Int)): Boolean =
    exists(pair._1, pair._2)

  def exists(y: Int, x: Int): Boolean =
    y >= 0 && x >= 0 && mapping.length >= y+1 && mapping(y).length >= x+1

  def getNeighbors(pair: (Int, Int)): Array[(Int, Int)] =
    getNeighbors(pair._1, pair._2)

  def getNeighbors(y: Int, x: Int): Array[(Int, Int)] =
    var builder = Set[(Int, Int)]()
    var offsets = Seq[(Int, Int)]((1, 0), (-1, 0), (0, 1), (0, -1))
    offsets.foreach(offset => if exists(y+offset._1, x+offset._2) then builder += (y+offset._1, x+offset._2))
    builder.toArray

  def getNeighborsDiagonal(pair: (Int, Int)): Array[(Int, Int)] =
    getNeighborsDiagonal(pair._1, pair._2)

  def getNeighborsDiagonal(y: Int, x: Int): Array[(Int, Int)] =
    var builder = Set[(Int, Int)]()
    var offsets = Seq[(Int, Int)]((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1))
    offsets.foreach(offset => if exists(y+offset._1, x+offset._2) then builder += (y+offset._1, x+offset._2))
    builder.toArray

  def getNeighborsWithInvalids(pair: (Int, Int)): Array[(Int, Int)] =
    getNeighborsWithInvalids(pair._1, pair._2)

  def getNeighborsWithInvalids(y: Int, x: Int): Array[(Int, Int)] =
    var builder = Set[(Int, Int)]()
    var offsets = Seq[(Int, Int)]((1, 0), (-1, 0), (0, 1), (0, -1))
    offsets.foreach(offset => builder += (y+offset._1, x+offset._2))
    builder.toArray

  // Setters
  def fill(value: A) =
    mapping = ArrayBuffer.fill(height)(ArrayBuffer.fill(width)(value))

  def set(pair: (Int, Int), value: A) =
    mapping(pair._1)(pair._2) = value

  def set(y: Int, x: Int, value: A) =
    mapping(y)(x) = value

  // All forms of getter
  def get(pair: (Int, Int)): A =
    mapping(pair._1)(pair._2)

  def get(y: Int, x: Int): A =
    mapping(y)(x)

  def get(y: Int): ArrayBuffer[A] =
    mapping(y)

  def apply(y: Int): ArrayBuffer[A] =
    mapping(y)

  def apply(y: Int, x: Int): A =
    mapping(y)(x)

  def apply(pair: (Int, Int)): A =
    mapping(pair._1)(pair._2)

  def getOption(pair: (Int, Int)): Option[A] =
    getOption(pair._1, pair._2)
  
  def getOption(y: Int, x: Int): Option[A] =
    if exists(y, x) then
      Some(mapping(y)(x))
    else
      None

  override def toString(): String = 
    var builder = ""
    for line <- mapping do
      builder += line.mkString
      builder += "\n"
    builder

  // Iterator to go over each coordinate pair in the grid
  def iterator: Iterator[(Int, Int)] =
    var builder = ArrayBuffer[(Int, Int)]()
    for (y <- 0 until height) do 
      for (x <- 0 until width) do 
        builder += ((y, x))
    builder.toIterator

}