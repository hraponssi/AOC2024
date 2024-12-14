import collection.mutable.ArrayBuffer
import collection.mutable.Set
import collection.mutable.Map

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global
import scala.compiletime.ops.double

//val D14Width = 11
//val D14Height = 7

val D14Width = 101
val D14Height = 103

var simulationTime = 100

var lowestRes = 999999999

class Robot(var pos: (Int, Int), var velocity: (Int, Int)) {
  override def toString(): String = 
    "Robot at " + pos + " with velocity " + velocity
}

class Solving14 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input14.txt").getLines().toVector

  // Part 1, and looped for part 2
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    // Load robots
    val robots = ArrayBuffer[Robot]()
    for li <- lines.indices do
      val line = lines(li)
      val split = line.split("\\=")
      robots += Robot((split(1).split("\\,")(0).toInt,split(1).split("\\,")(1).dropRight(2).toInt),
         (split(2).split("\\,")(0).toInt,split(2).split("\\,")(1).toInt))
    //println("loaded robots:" + robots.mkString("\n"))
    // Simulate them
    for robot <- robots do
      val cpos = robot.pos
      val vel = robot.velocity
      var newpos = ((cpos._1+vel._1*simulationTime) % D14Width, (cpos._2+vel._2*simulationTime) % D14Height)
      if (newpos._1 < 0) then
        newpos = (D14Width+newpos._1, newpos._2)
      if (newpos._2 < 0) then
        newpos = (newpos._1, D14Height+newpos._2)
      robot.pos = newpos
    // Count the robots in the quadrants
    val skipX = (D14Width-1)/2
    val skipY = (D14Height-1)/2
    val topleft = robots.count(robot => robot.pos._1 < skipX && robot.pos._2 < skipY)
    val topright = robots.count(robot => robot.pos._1 > skipX && robot.pos._2 < skipY)
    val bottomleft = robots.count(robot => robot.pos._1 < skipX && robot.pos._2 > skipY)
    val bottomright = robots.count(robot => robot.pos._1 > skipX && robot.pos._2 > skipY)
    // Count the result
    res = topleft*topright*bottomleft*bottomright
    val grid = Grid[Int](D14Height, D14Width, 0)
    robots.foreach(robot => grid(robot.pos._2)(robot.pos._1) += 1)
    //println(grid.toString().replaceAll("0", "."))
    res
    /* Uncomment for Part 2: Lowest "danger" = christmas tree shape
    if res < lowestRes then
      println(grid.toString().replaceAll("0", "."))
      Thread.sleep(2000)
      lowestRes = res.toInt
    simulationTime
    */    

  // Call for part 2
  def process2(lines: Vector[String]) =
    for i <- 0 to 10000 do
      println("Starting with: " + i)
      simulationTime = i 
      process(lines)

}