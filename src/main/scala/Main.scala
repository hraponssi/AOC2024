
// Main starter
object Main extends App {
  val solver = new Solving1()
  val lines = solver.readLines()
  //println("Input:")
  //println(lines.mkString("\n"))
  val result = solver.process(lines)
  println("result: " + result)
}