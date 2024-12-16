import collection.mutable.ArrayBuffer
import collection.mutable.Set
import collection.mutable.Map

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global
import scala.compiletime.ops.double

class Solving15 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input15.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    // Load the world
    val mapLines = lines.takeWhile(_ != "")
    val grid = Grid[Char](mapLines.size, mapLines(0).size*2, '.')
    var robot = (0,0)
    for li <- mapLines.indices do
      val line = lines(li)
      for ci <- line.indices do
        if line(ci) == '@' then
          robot = (li, ci*2)
          grid(li)(ci*2) = '.'
        else if line(ci) == '#' then
          grid(li)(ci*2) = line(ci)
          grid(li)(ci*2+1) = line(ci)
        else if line(ci) == 'O' then
          grid(li)(ci*2) = '['
          grid(li)(ci*2+1) = ']'
    println("Loaded grid:")
    grid(robot._1)(robot._2) = '@'
    println(grid)
    grid(robot._1)(robot._2) = '.'
    // Move the robot around
    val moves = lines.drop(mapLines.size).flatten
    var move = (0,0)
    for movechar <- moves do
      if (movechar == '^') then
        move = (-1,0)
      else if (movechar == '>') then
        move = (0,1)
      else if (movechar == 'v') then
        move = (1,0)
      else if (movechar == '<') then
        move = (0,-1)
      val newpos = (robot._1+move._1, robot._2+move._2)
      val path = grid.getInDirection(robot, move).takeWhile(grid(_) != '#')
      // The robot can only move if there is an empty space on the path
      // First index where 2 spots are empty
      if (movechar == '<' || movechar == '>') then
        val firstEmpty = path.indices.find(pos => grid(path(pos)) == '.')
        // Can it just directly move one place
        if path.nonEmpty && grid(path(0)) == '.' then
          robot = newpos
        // Else it has to move boxes
        else if firstEmpty.nonEmpty then
          // number of boxes to move
          val boxes = firstEmpty.get
          for ci <- path.take(boxes+1).indices do
            val coord = path.take(boxes+1).reverse(ci)
            if ci != boxes then
              grid(coord._1)(coord._2) = grid(path.take(boxes+1).reverse(ci+1))
            else
              grid(coord._1)(coord._2) = '.'
          robot = newpos
      else // Up or down movement
        // Helper to recursively check all the boxes the current box would move can move, or move them if doMoves
        def mover(start: (Int, Int), doMoves: Boolean): Boolean =
          var secondpos = (0,0)
          // Figure out which side the rest of this box is at
          var offset = 0
          if (grid(start) == '[') then
            offset = 1
          else
            offset = -1
          secondpos = (start._1, start._2+offset)
          // Find the first empty spot in the path of both parts of this box
          val path1 = grid.getInDirection(start, move).takeWhile(grid(_) != '#')
          val firstEmpty1 = path1.find(pos => grid(pos) == '.')
          val path2 = grid.getInDirection(secondpos, move).takeWhile(grid(_) != '#')
          val firstEmpty2 = path2.find(pos => grid(pos) == '.')
          // Both parts of this box have an empty space ahead
          if firstEmpty1.nonEmpty && firstEmpty2.nonEmpty then
            var firstPass = false
            var secondPass = false
            val next1 = path1(0)
            val next2 = path2(0)
            // If there is a box in the way recursively check it as well
            if firstEmpty1.get._1 != start._1+move._1 then
              if ((grid(next1) == '[' && offset == 1) || (grid(next1) == ']' && offset == -1)) then
                firstPass = mover(next1, doMoves)
                // If the next 2 spots are the same box no need to double check
                secondPass = firstPass
              else
                firstPass = mover(next1, doMoves)
            else
              firstPass = true
            // If there is a box in the way recursively check it as well
            if firstEmpty2.get._1 != start._1+move._1 && !secondPass then
              secondPass = mover(next2, doMoves)
            else
              secondPass = true
            // If both recursive checks succeeded and doMoves, move the box up one. (prior recursive calls moved the other boxes)
            if firstPass && secondPass && (doMoves) then
                grid(next1._1)(next1._2) = grid(start)
                grid(start._1)(start._2) = '.'
                grid(next2._1)(next2._2) = grid(secondpos)
                grid(secondpos._1)(secondpos._2) = '.'
            firstPass && secondPass
          else
            false
        val firstEmpty = path.indices.find(pos => grid(path(pos)) == '.')
        // If the boxes that would be moved can all be moved, move them
        if firstEmpty.nonEmpty && firstEmpty.get != 0 && mover(newpos, false) then
          //println("Could move it up or down")
          mover(newpos, true)
          robot = newpos
        else if firstEmpty.nonEmpty && firstEmpty.get == 0 then
          robot = newpos
    println("Final grid:")
    grid(robot._1)(robot._2) = '@'
    println(grid)
    println("Robot: " + robot)
    // Calculate result
    res = grid.filter(grid(_) == '[').map(pos => pos._1*100+pos._2).sum
    res

  // Part 1
  def process1(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    // Load the world
    val mapLines = lines.takeWhile(_ != "")
    val grid = Grid[Char](mapLines.size, mapLines(0).size, '.')
    var robot = (0,0)
    for li <- mapLines.indices do
      val line = lines(li)
      for ci <- line.indices do
        if line(ci) == '@' then
          robot = (li, ci)
          grid(li)(ci) = '.'
        else
          grid(li)(ci) = line(ci)
    println("Loaded grid:")
    println(grid)
    // Move the robot around
    val moves = lines.drop(mapLines.size).flatten
    var move = (0,0)
    for movechar <- moves do
      if (movechar == '^') then
        move = (-1,0)
      else if (movechar == '>') then
        move = (0,1)
      else if (movechar == 'v') then
        move = (1,0)
      else if (movechar == '<') then
        move = (0,-1)
      val newpos = (robot._1+move._1, robot._2+move._2)
      val path = grid.getInDirection(robot, move).takeWhile(grid(_) != '#')
      // The robot can only move if there is an empty space on the path
      val firstEmpty = path.find(grid(_) == '.')
      if firstEmpty.nonEmpty then
        // Number of boxes to move, if the free space is the next spot it moves nothing 
        val boxes = path.indexOf(firstEmpty.get)
        for ci <- path.take(boxes+1).indices do
          val coord = path.take(boxes+1).reverse(ci)
          if ci != boxes then
            grid(coord._1)(coord._2) = grid(path.take(boxes+1).reverse(ci+1))
          else
            grid(coord._1)(coord._2) = '.'
        robot = newpos
    // Calculate result
    res = grid.filter(grid(_) == 'O').map(pos => pos._1*100+pos._2).sum
    res

}