import collection.mutable.ArrayBuffer
import collection.mutable.Set

class Solving8 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input.txt").getLines().toVector

  // Part 1
  def process(lines: Vector[String]): Int =
    var res: Int = 0
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        
    res

}