import collection.mutable.ArrayBuffer
import collection.mutable.Set

class Solving8 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input8.txt").getLines().toVector

  // Print the map
  def printMap(ydim: Int, xdim: Int, points: ArrayBuffer[(Int, Int)]) =
    for y <- (0 to ydim) do
      for x <- (0 to xdim) do
        if points.contains((y, x)) then
          print("X")
        else
          print(".")
      println("")
  
  // Part 2
  def process(lines: Vector[String]): Int =
    var antennas = ArrayBuffer[(Int, Int, Char)]()
    var antinodes = ArrayBuffer[(Int, Int, Char)]()

    // Helper to check if a point is in the map
    def isValid(y: Int, x: Int) =
      x >= 0 && x < lines(0).length() && y >= 0 && y < lines.length

    // Load antennas
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        if (char != '.') then
          antennas += ((li, ci, char))

    // Find antinodes
    val chars = antennas.map(_._3).distinct
    for t <- chars do
      val chats = antennas.filter(_._3 == t)
      for char <- chats do
        for other <- chats.filter(_ != char) do
          val (y, x) = (char._1, char._2)
          val (oy, ox) = (other._1, other._2)
          val (diffy, diffx) = ((y-oy), (x-ox))
          for mul <- (1 to 1000) do
            if isValid(y+diffy*mul, x+diffx*mul) then
              antinodes += ((y+diffy*mul, x+diffx*mul, t))

    (antinodes ++ antennas).map(en => (en._1, en._2)).distinct.size

  // Part 1
  def process1(lines: Vector[String]): Int =
    var antennas = ArrayBuffer[(Int, Int, Char)]()
    var antinodes = ArrayBuffer[(Int, Int, Char)]()

    // Helper to check if a point is in the map
    def isValid(y: Int, x: Int) =
      x >= 0 && x < lines(0).length() && y >= 0 && y < lines.length

    // Load antennas
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        if (char != '.') then
          antennas += ((li, ci, char))

    // Find antinodes
    val chars = antennas.map(_._3).distinct
    for t <- chars do
      val chats = antennas.filter(_._3 == t)
      for char <- chats do
        for other <- chats.filter(_ != char) do
          val (y, x) = (char._1, char._2)
          val (oy, ox) = (other._1, other._2)
          val (diffy, diffx) = ((y-oy), (x-ox))
          if isValid(y+diffy, x+diffx) then
            antinodes += ((y+diffy, x+diffx, t))

    //printMap(lines.size, lines(0).size, antinodes.map(en => (en._1, en._2)).distinct)

    antinodes.map(en => (en._1, en._2)).distinct.size
}