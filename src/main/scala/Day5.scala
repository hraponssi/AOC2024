class Solving5 {
  def readLines() =
    scala.io.Source.fromFile("input5.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Int =
    var res = 0
    var good = collection.mutable.Buffer[Array[Int]]()
    var pairs = collection.mutable.Buffer[(Int, Int)]()
    // Pairs
    for line <- lines.takeWhile(_ != "") do
      val split = line.split("\\|").map(_.toInt)
      pairs += ((split(0), split(1)))
      
    // Recursively fix a line
    def processLine(line: Array[Int], first: Boolean): Unit =
      def swap(fi: Int, si: Int) =
        var nline = line.clone()
        nline(fi) = line(si)
        nline(si) = line(fi)
        nline
      var builder = collection.mutable.Buffer[Int]()
      for i <- line.indices do
        val num = line(i)
        val followers = pairs.filter(_._1 == num).map(_._2)
        // if the number was passed already
        for fo <- followers do
          if builder.contains(fo) then
            return processLine(swap(line.indexOf(fo), i), false)
        builder += num
      if !first then
        good += line

    var end = lines.indexOf("")
    // Number lists, with a fun oneliner
    lines.drop(end+1).foreach(line => processLine(line.split(",").map(_.toInt), true))
    /*
    for line <- lines.drop(end+1) do
      val split = line.split(",").map(_.toInt)
      processLine(split, true)
    */
    
    // Add the fixed lines together
    for nums <- good do
      res += nums(nums.length/2)
      //println(nums.mkString(".."))
    res

  // Part 1
  def process1(lines: Vector[String]): Int =
    var res = 0
    var good = collection.mutable.Buffer[String]()
    var pairs = collection.mutable.Buffer[(Int, Int)]()
    // Pairs
    for line <- lines.takeWhile(_ != "") do
      val split = line.split("\\|").map(_.toInt)
      pairs += ((split(0), split(1)))
      
    var end = lines.indexOf("")
    // Number lists
    for line <- lines.drop(end+1) do
      val split = line.split(",").map(_.toInt)
      var builder = collection.mutable.Buffer[Int]()
      var bad = false
      for num <- split do
        val followers = pairs.filter(_._1 == num).map(_._2)
        // if the number was passed already
        bad = bad || followers.foldLeft(false)((tracker, next) => (builder.contains(next) || tracker))
        builder += num
      if !bad then
        good += line
    
    // Add the good lines together
    for line <- good do
      val nums = line.split(",").map(_.toInt)
      res += nums(nums.length/2)
    //println(good.mkString("\n"))
    res
}