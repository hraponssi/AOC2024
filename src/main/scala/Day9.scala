import collection.mutable.ArrayBuffer
import collection.mutable.Set

import scala.util.control.Breaks._

class Solving9 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input9.txt").getLines().toVector
  
  // Part 2
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    var memory = ArrayBuffer[Int]()
    val line = lines(0)
    // Load up memory
    for ci <- line.indices do
      val char = line(ci)
      for i <- 0 until char.asDigit do
        if ci % 2 == 0 then
          memory += ci/2
        else
          memory += -1
    // Sort
    var j = memory.length-1
    while memory(j) == -1 do
      j -= 1
    while j > 0 do
      val id = memory(j)
      val len = memory.dropRight(memory.length-1-j).reverse.takeWhile(_ == id).length
      var done = false
      var slider = memory.clone().sliding(len).zipWithIndex
      // Slide a window the length of the chunk to be moved over the whole memory
      // Uses a breakable to break out if continuing is unnecessary, providing some speedup
      breakable {
      for (next, in) <- slider do
        if next.forall(_ == -1) then
          var reader = in
          for i <- j-len+1 to j do
            memory(i) = -1
            memory(reader) = id
            reader += 1
          done = true
        if done || in >= j-len+1 then
          break()
      }
      while j >= 0 && (memory(j) == -1 || memory(j) == id) do
        j -= 1
      // println("Indexes remaining: " + j)
    // Print out memory
    // println(memory.mkString.replaceAll("-1", "-"))
    // Calculate answer
    memory.indices.filter(memory(_) != -1).foreach(in => res += memory(in) * in)
    res

  // Part 1
  def process1(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    var memory = ArrayBuffer[Int]()
    val line = lines(0)
    // Load up memory
    for ci <- line.indices do
      val char = line(ci)
      for i <- 0 until char.asDigit do
        if ci % 2 == 0 then
          memory += ci/2
        else
          memory += -1
    // Sort
    var i = 0
    var j = memory.length-1
    while memory(j) == -1 do
      j -= 1
    while i < j do
      if memory(i) == -1 then
        memory(i) = memory(j)
        memory(j) = -1
        while memory(j) == -1 do
          j -= 1
      i += 1
    // Print out memory
    // println(memory.mkString.replaceAll("-1", "-"))
    // Calculate answer
    memory.indices.filter(memory(_) != -1).foreach(in => res += memory(in) * in)
    res
}