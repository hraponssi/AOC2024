class Solving2 {
  def readLines() =
    scala.io.Source.fromFile("input2.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Int =
    var res = 0
    def check(split: Array[String]): Boolean =
      val allminus = split.indices.drop(1).forall(i => (split(i-1).toInt - split(i).toInt) < 0)
      val allplus = split.indices.drop(1).forall(i => (split(i-1).toInt - split(i).toInt) > 0)
      val maxdiff = split.indices.drop(1).forall(i => Math.abs(split(i-1).toInt - split(i).toInt) <= 3)
      (allminus || allplus) && maxdiff
    for line <- lines do
      var good = false
      val split = line.split(" ")
      // "A nice oneliner"
      good = split.indices.filter(i => check(split.indices.filter(_ != i).map(split(_)).toArray)).length > 0
      /*
      for i <- split.indices do
        if check(split.indices.filter(_ != i).map(split(_)).toArray) then
          good = true
      */
      if good then 
        res += 1
    res

  // Part 1
  def process1(lines: Vector[String]): Int =
    var res = 0
    def check(split: Array[String]): Boolean =
      val allminus = split.indices.drop(1).forall(i => (split(i-1).toInt - split(i).toInt) < 0)
      val allplus = split.indices.drop(1).forall(i => (split(i-1).toInt - split(i).toInt) > 0)
      val maxdiff = split.indices.drop(1).forall(i => Math.abs(split(i-1).toInt - split(i).toInt) <= 3)
      (allminus || allplus) && maxdiff
    for line <- lines do
      val split = line.split(" ")
      if check(split) then 
        res += 1
    res
}