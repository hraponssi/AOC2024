class Solving4 {
  def readLines() =
    scala.io.Source.fromFile("input4.txt").getLines().toVector

  def process(lines: Vector[String]): Int =
    var res = 0
    val offsets = collection.mutable.Buffer[Vector[(Int, Int, Char)]]()
    // Part 1
    /*
    offsets += Vector[(Int, Int, Char)]((0, 0, 'X'), (1, 0, 'M'), (2, 0, 'A'), (3, 0, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'X'), (0, 1, 'M'), (0, 2, 'A'), (0, 3, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'X'), (1, 1, 'M'), (2, 2, 'A'), (3, 3, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'X'), (-1, 1, 'M'), (-2, 2, 'A'), (-3, 3, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (1, 0, 'A'), (2, 0, 'M'), (3, 0, 'X'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (0, 1, 'A'), (0, 2, 'M'), (0, 3, 'X'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (1, 1, 'A'), (2, 2, 'M'), (3, 3, 'X'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (-1, 1, 'A'), (-2, 2, 'M'), (-3, 3, 'X'))
    */
    // Part 2
    offsets += Vector[(Int, Int, Char)]((0, 0, 'M'), (1, 1, 'A'), (2, 2, 'S'), (0, 2, 'M'), (2, 0, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (1, 1, 'A'), (2, 2, 'M'), (0, 2, 'S'), (2, 0, 'M'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'S'), (1, 1, 'A'), (2, 2, 'M'), (0, 2, 'M'), (2, 0, 'S'))
    offsets += Vector[(Int, Int, Char)]((0, 0, 'M'), (1, 1, 'A'), (2, 2, 'S'), (0, 2, 'S'), (2, 0, 'M'))
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        for offlist <- offsets do
          var good = true
          for placement <- offlist do
            val cx = ci+placement._1
            val cy = li+placement._2
            val char = placement._3
            if (cx >= 0 && cx < line.length && cy >= 0 && cy < lines.length) then
              if lines(cy)(cx) != char then
                good = false
            else
              good = false
          if good then
            res += 1
      
    res
}