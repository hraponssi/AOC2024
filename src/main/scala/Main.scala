
// Main starter
object Main extends App {
  def run() =
    val solver = new Solving17()
    val lines = solver.readLines()
    val result = solver.process(lines)
    println("result: " + result)
  run()
}

object MeasureTime extends App {
  val startTime = System.currentTimeMillis()
  Main
  println("Run time total: " + (System.currentTimeMillis()-startTime) + "ms")
}