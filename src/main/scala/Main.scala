
// Main starter
object Main extends App {
  def run() =
    val solver = new Solving14()
    val lines = solver.readLines()
    val result = solver.process(lines)
    println("result: " + result)
}

object MeasureTime extends App {
  val startTime = System.currentTimeMillis()
  Main.run()
  println("Run time total: " + (System.currentTimeMillis()-startTime) + "ms")
}