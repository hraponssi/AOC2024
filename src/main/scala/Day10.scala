import collection.mutable.ArrayBuffer
import collection.mutable.Set

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

class Solving10 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input10.txt").getLines().toVector

  // Part 1 & 2
  def process(lines: Vector[String]): Int =
    var res = 0
    var map: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(lines.size)(ArrayBuffer.fill(lines(0).length())(0))
    var startingPoints = ArrayBuffer[(Int, Int)]()
    // Load map and start points
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        val num = char.asDigit
        map(li)(ci) = num
        if num == 0 then
          startingPoints += ((li, ci))
    // Helper to get tile neighbors
    def getNeighbors(point: (Int, Int)) =
      val (py, px) = point
      // A funny one liner that can be inefficient if the map gets very big
      // Gather each Y point
      map.indices.filter(i => i == py+1 || i == py || i == py-1)
        .map(in => map(in)
          // Gather each X point
          .indices.filter(i => i == px+1-Math.abs(py-in) || i == px-1+Math.abs(py-in))
            // Combine the Y and X points, skip the midpoint
            .map((in, _))).flatten.filter(_ != point)
    // For part 1 this list prevents double counting
    var reached = Map[(Int, Int), Set[(Int, Int)]]().withDefaultValue(Set())
    // Returns the amount of 9 points reached from a point
    def pathFind(path: Array[(Int, Int)], cweight: Int): Int =
      val point = path.last
      val neighbors = getNeighbors(point)
      if cweight == 9 then
        // For part 1 uncomment this line:
        // reached(path.head) += point

        // println("Finished by path " + path.map(p => map(p._1)(p._2)).mkString(","))
        return 1
      neighbors.filter(
        n => cweight+1 < 10
        && map(n._1)(n._2) == cweight+1
        && !reached(path.head).contains(n)
        ).map(n => pathFind(path :+ n, cweight+1)).sum
    // Simulate from each start point
    for sp <- startingPoints do
      val (sy, sx) = sp
      res += pathFind(Array(sp), 0)
    
    // Multithreaded version doesnt really make gains with such a small input
    // var tasks = startingPoints.map(sp => Future {pathFind(Array(sp), 0)})
    // res = Await.result(Future.sequence(tasks), 30.seconds).sum
    res

}