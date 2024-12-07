class Solving7 {
  def readLines() =
    scala.io.Source.fromFile("input7.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    for li <- lines.indices do
      val target = lines(li).split(": ")(0).toDouble
      val numbers = lines(li).split(": ")(1).split(" ").map(_.toDouble)

      // Recursively try each operation if it stays below the target
      def solver(base: BigDecimal, remaining: Array[Double]): Boolean =
        if remaining.size == 0 then
          return false
        val next = remaining(0)
        var ret = false
        // Multiply with next number
        val multiplied = base*next
        if (multiplied <= target && !ret) then
          if (multiplied == target && remaining.size == 1) then
            ret = true
          else
            ret = solver(multiplied, remaining.drop(1))
        // Add with next number
        val added = base+next
        if (added <= target && !ret) then
          if (added == target && remaining.size == 1) then
            ret = true
          else
            ret = solver(added, remaining.drop(1))
        // The || operator to just append the next number
        val baseStr = base.toString.split("\\.")(0)
        val appended = BigDecimal(baseStr+next.toString)
        if (appended <= target && !ret) then
          if (appended == target && remaining.size == 1) then
            ret = true
          else
            ret = solver(appended, remaining.drop(1))
        ret
        
      if solver(numbers(0), numbers.drop(1)) then
        res += target
    res

  // Part 1
  def process1(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    for li <- lines.indices do
      val target = lines(li).split(": ")(0).toDouble
      val numbers = lines(li).split(": ")(1).split(" ").map(_.toDouble)

      // Recursively try each operation if it stays below the target
      def solver(base: BigDecimal, remaining: Array[Double]): Boolean =
        if remaining.size == 0 then
          return false
        val next = remaining(0)
        var ret = false
        // Multiply with next number
        if (base*next <= target && !ret) then
          if (base*next == target && remaining.size == 1) then
            ret = true
          else
            ret = solver(base*next, remaining.drop(1))
        // Add with next number
        if (base+next <= target && !ret) then
          if (base+next == target && remaining.size == 1) then
            ret = true
          else
            ret = solver(base+next, remaining.drop(1))
        ret

      if solver(numbers(0), numbers.drop(1)) then
        res += target
    res

}