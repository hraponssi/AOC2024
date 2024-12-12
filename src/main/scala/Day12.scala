import collection.mutable.ArrayBuffer
import collection.mutable.Set
import collection.mutable.Map

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global
import scala.compiletime.ops.double

class Solving12 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input12.txt").getLines().toVector

  // Part 2
  def process(lines: Vector[String]): Long =
    var res: Long = 0
    // Load the map into the grid helper object
    val map: Grid[Char] = Grid(lines.size, lines(0).size, 'X')
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        map(li)(ci) = char
    // Load the gardens from the grid
    var gardens = Vector[Vector[(Int, Int)]]()
    var visited = Set[(Int, Int)]()
    for coord <- map do
      var collecter = Set[(Int, Int)](coord)
      val c = map(coord)
      // Helper recursive function to find all the connected tiles of the same type
      def search(scoord: (Int, Int)): Unit =
        for neighbor <- map.getNeighbors(scoord) do
          if map(neighbor) == c && !visited.contains(neighbor) then
            visited += neighbor
            collecter += neighbor
            search(neighbor)
      if !visited.contains(coord) then
        search(coord)
        visited += coord
        gardens = gardens :+ collecter.toVector
    // Calculate the fence cost for each garden
    val directions = Vector[(Int, Int)]((0,1),(0,-1),(1,0),(-1,0))
    for garden <- gardens do
      val borders = Set[(Int, Int, Int)]()
      var counter = 0
      // Build a list of the bordering tiles with directions
      for tile <- garden do 
        val c = map(tile)
        for di <- directions.indices do
          val dir = directions(di)
          val point = (tile._1+dir._1, tile._2+dir._2)
          val use = (point._1, point._2, di)
          if !borders.contains(use) && (map.getOption(point) == None || map(point) != c) then
            borders += use
      // Helper recursive function to remove all the border tiles sharing this fence
      def search(point: (Int, Int, Int)): Unit =
        for neighbor <- map.getNeighborsWithInvalids(point._1, point._2) do
          val usePoint = (neighbor._1, neighbor._2, point._3)
          if borders.contains(usePoint) then
            borders.remove(usePoint)
            search(usePoint)
      // Loop the bordering tiles until all have a fence
      while !borders.isEmpty do
        val edgeUse = borders.head
        borders.remove(edgeUse)
        search(edgeUse)
        counter += garden.size
      // println("cost for region " + map(garden(0)) + " is " + counter + " with garden size " + garden.size + " sides must be " + counter/garden.size)
      res += counter
    res

  // Part 1
  def process1(lines: Vector[String]): Long =
    var res: Long = 0
    val map: Grid[Char] = Grid(lines.size, lines(0).size, 'X')
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        map(li)(ci) = char
    // Load the gardens from the grid
    var gardens = Vector[Vector[(Int, Int)]]()
    var visited = Set[(Int, Int)]()
    for coord <- map do
      var collecter = Set[(Int, Int)](coord)
      val c = map(coord)
      def search(scoord: (Int, Int)): Unit =
        for neighbor <- map.getNeighbors(scoord) do
          if map(neighbor) == c && !visited.contains(neighbor) then
            visited += neighbor
            collecter += neighbor
            search(neighbor)
      if !visited.contains(coord) then
        search(coord)
        visited += coord
        gardens = gardens :+ collecter.toVector
    // Calculate the fence cost for each garden
    for garden <- gardens do
      var counter = 0
      for tile <- garden do 
        val c = map(tile)
        counter += garden.size * map.getNeighborsWithInvalids(tile).filter(!garden.contains(_)).size
      // println("cost for region " + map(garden(0)) + " is " + counter + " with garden size " + garden.size + " sides must be " + counter/garden.size)
      res += counter
    res

}