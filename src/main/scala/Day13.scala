import collection.mutable.ArrayBuffer
import collection.mutable.Set
import collection.mutable.Map

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

import concurrent._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global
import scala.compiletime.ops.double

class Machine(val buttonA: (Int, Int), val buttonB: (Int, Int), val goal: (Int, Int)) {
  override def toString(): String = 
    "Button A: " + buttonA + " Button B: " + buttonB + " Prize: " + goal
}

class Solving13 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input.txt").getLines().toVector

  // Part 1 & 2
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    // For part 1 set this offest to 0:
    val offset = 10000000000000l
    // Load machines
    var machines = ArrayBuffer[Machine]()
    var ba: (Int, Int) = (0, 0)
    var bb: (Int, Int) = (0, 0)
    var pr: (Int, Int) = (0, 0)
    for li <- lines.indices do
      val line = lines(li)
      val split = line.split(" ")
      if (li%4 == 0) then
        // Button A
        ba = (split(2).drop(2).dropRight(1).toInt, split(3).drop(2).toInt)
      else if (li%4 == 1) then
        // Button B
        bb = (split(2).drop(2).dropRight(1).toInt, split(3).drop(2).toInt)
      else if (li%4 == 2) then
        // Prize
        pr = (split(1).drop(2).dropRight(1).toInt, split(2).drop(2).toInt)
      if (li%4 == 3) || li == lines.size-1 then
        machines += Machine(ba, bb, pr)
    // println("Loaded machines: " + machines.mkString("\n"))
    // Calculate the A and B combination for each machine
    for machine <- machines do
      val gx = BigDecimal(machine.goal._1)+offset
      val gy = BigDecimal(machine.goal._2)+offset
      val buttonAX = BigDecimal(machine.buttonA._1)
      val buttonAY = BigDecimal(machine.buttonA._2)
      val buttonBX = BigDecimal(machine.buttonB._1)
      val buttonBY = BigDecimal(machine.buttonB._2)
      val apresses = (gx-gy*(buttonBX/buttonBY))/(buttonAX-buttonAY*(buttonBX/buttonBY))
      val bpresses = (gx-gy*(buttonAX/buttonAY))/(buttonBX-buttonBY*(buttonAX/buttonAY))
      val result = (apresses, bpresses)
      // println("Result: " + result)
      // Check if the result is a whole number, with floating point error
      if ((apresses-apresses.setScale(8, RoundingMode.HALF_UP)).abs < 0.00000000000000000001) then
        res += (result._1*3 + result._2)

    res

}