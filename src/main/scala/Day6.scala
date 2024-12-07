import collection.mutable.ArrayBuffer
import collection.mutable.Set

class Solving6 {
  def readLines() =
    scala.io.Source.fromFile("input6.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Int =
    var res = 0
    var grid: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(lines.size)(ArrayBuffer.fill(lines(0).length())(0))
    var visited = Set[(Int, Int, Int)]()
    var guard = (0, 0, 1)
    // Load map
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        if char == '#' then
          grid(li)(ci) = 1
        else if char == '^' then
          guard = (li, ci, 1)
          visited += ((li, ci, 1))
          grid(li)(ci) = 0
        else
          grid(li)(ci) = 0
    val guardstart = guard
    // Pathing function
    def attempt(): Boolean =
      // Move the guard. Rotates until an open direction is found
      def moveGuard(): Boolean = // True if reached map edge
        val rotation = if guard._3 < 4 then guard._3+1 else 1
        val newpos = 
          if guard._3 == 1 then
            (guard._1-1, guard._2, guard._3)
          else if guard._3 == 2 then
            (guard._1, guard._2+1, guard._3)
          else if guard._3 == 3 then
            (guard._1+1, guard._2, guard._3)
          else
            (guard._1, guard._2-1, guard._3)
        if (newpos._1 < 0 || newpos._2 < 0 || newpos._1 > grid.length-1 || newpos._2 > grid(0).length-1) then
          true
        else if (grid(newpos._1)(newpos._2) == 0) then
          guard = newpos
          false
        else
          guard = (guard._1, guard._2, rotation)
          moveGuard()
      var fail = false
      var looped = false
      moveGuard()
      while !fail do
        fail = moveGuard()
        val pos = (guard)
        //println("move to " + pos)
        if !visited.contains(pos) then
          visited += pos
        else if !fail then
          fail = true
          looped = true
      looped
    // Go over the path, adding obstacles on each spot and look for a loop
    attempt() // Get initial path
    val unique = ArrayBuffer[(Int, Int)]()
    val fullpath = visited.clone()
    val uniquevisited = visited.map(triple => (triple._1, triple._2)).filter(en => 
      if (unique.contains(en)) false 
      else 
        unique += en
        true
      )
    for spot <- uniquevisited do
      //println("doing spot " + spot)
      visited.clear()
      guard = guardstart
      grid(spot._1)(spot._2) = 1
      if attempt() then
        //println("counted block at " + spot)
        res += 1
      grid(spot._1)(spot._2) = 0
    res

  // Part 1
  def process1(lines: Vector[String]): Int =
    var grid: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(lines.size)(ArrayBuffer.fill(lines(0).length())(0))
    var visited = Set[(Int, Int)]()
    var guard = (0, 0, 1)
    // Load map
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        if char == '#' then
          grid(li)(ci) = 1
        else if char == '^' then
          guard = (li, ci, 1)
          visited += ((li, ci))
          grid(li)(ci) = 0
        else
          grid(li)(ci) = 0
    def moveGuard(): Boolean = // True if reached edge
      val rotation = if guard._3 < 4 then guard._3+1 else 1
      val newpos = 
        if guard._3 == 1 then
          (guard._1-1, guard._2, guard._3)
        else if guard._3 == 2 then
          (guard._1, guard._2+1, guard._3)
        else if guard._3 == 3 then
          (guard._1+1, guard._2, guard._3)
        else
          (guard._1, guard._2-1, guard._3)
      if (newpos._1 < 0 || newpos._2 < 0 || newpos._1 > grid.length-1 || newpos._2 > grid(0).length-1) then
        true
      else if (grid(newpos._1)(newpos._2) == 0) then
        guard = newpos
        false
      else
        guard = (guard._1, guard._2, rotation)
        moveGuard()
    while !moveGuard() do
      val pos = ((guard._1, guard._2))
      //println("move to " + pos)
      if !visited.contains(pos) then
        visited += pos
    visited.size
}