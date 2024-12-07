class Solving3 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input3.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Int =
    var res = 0
    var disable = false
    for line <- lines do
      val fs = line.split("mul\\(")
      var first = true
      for cand <- fs do
        val ss = cand.split("\\)")
        if !disable && !first then
          if ss(0).size > 0 then
            val finals = ss(0).split(",")
            if finals.length == 2 then
              val fi = finals(0).toIntOption
              val f2 = finals(1).toIntOption
              res += fi.getOrElse(0)*f2.getOrElse(0)
        if cand.length() >= 7 then
          val last = cand.indices.drop(6).map(in => cand.substring(in-6, in+1)).findLast(check => check.contains("do()") || check.contains("don't()"))
          if last.getOrElse("").contains("do()") then
            disable = false
          if last.getOrElse("").contains("don't()") then
            disable = true
        else
          if cand.contains("do()") then
            disable = false
        first = false
    res

  // Part 1
  def process1(lines: Vector[String]): Int =
    var res = 0
    for line <- lines do
      val fs = line.split("mul\\(")
      for cand <- fs do
        val ss = cand.split("\\)")
        if ss(0).size > 0 then
          val finals = ss(0).split(",")
          if finals.length == 2 then
            val fi = finals(0).toIntOption
            val f2 = finals(1).toIntOption
            res += fi.getOrElse(0)*f2.getOrElse(0)
    res
}