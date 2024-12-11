import collection.mutable.ArrayBuffer
import collection.mutable.Set
import collection.mutable.Map

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

class Solving11 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input11.txt").getLines().toVector

  // Part 1 & 2
  def process(lines: Vector[String]): Long =
    var res: Long = 0
    var rocks = List[Long]()
    // Load rocks
    for li <- lines.indices do
      val line = lines(li)
      val split = line.split(" ")
      for num <- split do
        rocks = rocks :+ num.toInt
    // Recursive simulation of the rocks, skipping rock step combos already calculated
    var precalculated = Map[(Long, Long), Long]()
    def blink(rock: Long, steps: Int): Long =
      if precalculated.contains((rock, steps)) then
        return precalculated((rock, steps))
      if steps == 0 then
        return 1
      val stringified = rock.toString()
      val length = stringified.length()
      var result: Long = 0
      if rock == 0 then
        result = blink(rock+1, steps-1)
      else if length % 2 == 0 then
        result = blink(stringified.dropRight(length/2).toLong, steps-1) + blink(stringified.drop(length/2).toLong, steps-1)
      else
        result = blink(rock*2024, steps-1)
      precalculated.put((rock, steps), result)
      result
    // Call the simulation for each rock
    def callrock(i: Int) =
      val calc = blink(rocks(i), 75)
      println("Finished rock " + i)
      calc
    /* Multithreaded doesnt work with the precalculated list, it causes data races
    var tasks = ArrayBuffer[Future[Long]]()
    for i <- rocks.indices do
      tasks += Future {callrock(i)}
    res = Await.result(Future.sequence(tasks), 1.hour).sum
    */
    res = rocks.indices.map(callrock(_)).sum
    res

}