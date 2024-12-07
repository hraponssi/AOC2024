
// Main starter
object Main extends App {
  val solver = new Solving8()
  val lines = solver.readLines()
  //println("Input:")
  //println(lines.mkString("\n"))
  val result = solver.process(lines)
  println("result: " + result)
}

object MeasureTime extends App {
  val startTime = System.currentTimeMillis()
  Main
  println("Run time total: " + (System.currentTimeMillis()-startTime) + "ms")
}