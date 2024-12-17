import collection.mutable.ArrayBuffer

class Solving17 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input17.txt").getLines().toVector

  val threebits: Long = 7

  // Part 1
  def process(lines: Vector[String]): String =
    var res = ""

    // Load the machine
    val registers = ArrayBuffer[Long]()
    val program = ArrayBuffer[Int]()

    registers.addAll(lines.filter(_.contains(":")).take((3)).map(_.split(": ")(1).toLong))
    program.addAll(lines.last.split(": ")(1).split(",").map(_.toInt))

    var pointer = 0

    // Helper to print the machine state
    def printState() =
      println("Registers: " + registers.mkString(","))
      println("Program: " + program.mkString(","))

    //printState()

    // Helper to get combo
    def getCombo(operand: Int): Long =
      if operand < 4 then
        operand
      else if operand < 7 then
        registers(operand-4)
      else -1

    // Collector for out instructions
    val out = ArrayBuffer[String]()

    // Run the program until the pointer goes past the last instruction
    while pointer < program.size do
      val instruction = program(pointer)
      var skipjump = false
      instruction match
        case 0 =>
          registers(0) = Math.floor(registers(0).toFloat/(1 << getCombo(program(pointer+1))).toFloat).toLong //& threebits
        case 1 =>
          registers(1) = registers(1) ^ program(pointer+1)
        case 2 =>
          registers(1) = getCombo(program(pointer+1)) & threebits
        case 3 =>
          if registers(0) != 0 then
            pointer = program(pointer+1)
            skipjump = true
        case 4 =>
          registers(1) = registers(1) ^ registers(2)
        case 5 =>
          out += (getCombo(program(pointer+1)) & threebits).toString()
        case 6 =>
          registers(1) = (registers(0)/(1 << getCombo(program(pointer+1)))) & threebits
        case 7 =>
          registers(2) = (registers(0)/(1 << getCombo(program(pointer+1)))) & threebits
      if !skipjump then
        pointer += 2
    //printState()

    res = out.mkString(",")

    res

}