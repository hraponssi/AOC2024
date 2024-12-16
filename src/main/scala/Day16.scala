import collection.mutable.Map

class Solving16 {
  def readLines() =
    scala.io.Source.fromFile("inputs/input16.txt").getLines().toVector

  // Part 1
  def process(lines: Vector[String]): BigDecimal =
    var res: BigDecimal = 0
    // Load the grid
    val grid = Grid[Char](lines.size, lines(0).size, '.')
    var start = (0,0)
    var end = (0,0)
    for li <- lines.indices do
      val line = lines(li)
      for ci <- line.indices do
        val char = line(ci)
        grid(li)(ci) = char
        if char == 'S' then
          start = (li, ci)
        else if char == 'E' then
          end = (li, ci)
    println("Loaded grid:\n" + grid.toString())
    // Find the lowest weight route
    val weights = Map[(Int, Int), Int]()
    val parents = Map[(Int, Int), (Int, Int)]()
    var queue = collection.mutable.PriorityQueue.empty[((Int, Int), Int)](
      Ordering.by((_ : ((Int, Int), Int))._2).reverse
    ) // vertex, weight so far
    queue.enqueue((start, 0))
    weights += start -> 0
    // To simulate the start point facing east, make its parent be west
    parents += start -> (start._1, start._2-1)
    var stop = false
    // Go through the priority queue, lowest weights first, until the first solution (lowest weight) is found
    while queue.nonEmpty && !stop do
      val next = queue.dequeue()
      val pos = next._1
      val weight = next._2
      val parent = parents(pos)
      // If this is the end position we found the solution
      if pos == end then
        stop = true
        println("A final point " + weight)
      for neighbor <- grid.getNeighbors(pos).filter(grid(_) != '#') do
        var cweight = weights.getOrElse(neighbor, Int.MaxValue)
        // Very hacky way to consider a mandatory turn in the neighbors current weight, in case this path already did that turn and is better
        val neighborparent = parents.getOrElse(neighbor, neighbor)
        val neighborPrior = grid.getOption(neighbor._1+(neighbor._1-neighborparent._1), neighbor._2+(neighbor._2-neighborparent._2))
        if neighborPrior.getOrElse('x') == '#' then
          cweight += 1000
        var cost = 1
        // The neighbor requires a turn
        if (parent._1-pos._1 != pos._1-neighbor._1 || parent._2-pos._2 != pos._2-neighbor._2) then
          cost += 1000
        val nweight = weight+cost
        if nweight < cweight then
          parents(neighbor) = pos
          weights(neighbor) = nweight
          queue.enqueue((neighbor, nweight))
    // Draw the path on the grid
    var parent = parents(end)
    while parent != parents(start) do
      grid(parent._1)(parent._2) = 'X'
      parent = parents(parent)
    println("Done grid:\n" + grid.toString())
    res = weights(end)
    res

}