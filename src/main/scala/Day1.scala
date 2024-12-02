class Solving1 {
  def readLines() =
    scala.io.Source.fromFile("input1.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Int =
    var res = 0
    var left = collection.mutable.ListBuffer[Int]()
    var right = collection.mutable.ListBuffer[Int]()
    left.addAll(lines.map(_.split("   ")(0).toInt))
    right.addAll(lines.map(_.split("   ")(1).toInt))
    val rsort = right.sorted
    val lsort = left.sorted
    for i <- lsort.indices do
      res += lsort(i)*rsort.count(_ == lsort(i))
    res

  // Single Line Part 2
  def process2l(lines: Vector[String]): Int =
    lines.map(_.split("   ")(0).toInt).map(v => v*lines.map(_.split("   ")(1).toInt).count(_ == v)).sum

  // Part 1
  def process1(lines: Vector[String]): Int =
    var res = 0
    var left = collection.mutable.ListBuffer[Int]()
    var right = collection.mutable.ListBuffer[Int]()
    left.addAll(lines.map(_.split("   ")(0).toInt))
    right.addAll(lines.map(_.split("   ")(1).toInt))
    val rsort = right.sorted
    val lsort = left.sorted
    for i <- rsort.indices do
      res += Math.abs(rsort(i)-lsort(i))
    res
}