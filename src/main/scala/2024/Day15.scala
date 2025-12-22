package `2024`

@main def day15(): Unit =

  val input = io.Source.fromResource("2024/day-15.txt").getLines.toList

  val grid = input.takeWhile(_.nonEmpty).toVector

  import aoc.{Point, Area, Dir}

  val area = Area(grid)

  val movements: List[Dir] = input.drop(area.height + 1).flatMap: line =>
    line.map:
      case '^' => Dir.N
      case 'v' => Dir.S
      case '<' => Dir.W
      case '>' => Dir.E

  val startPos =
    val s = area.pointsIterator.find(grid(_) == '@').get
    s.copy(x = s.x * 2)

  val startBoxes =
    area.pointsIterator.filter(grid(_) == 'O').toSet
      .map(b => b.copy(x = b.x * 2))

  val walls =
    area.pointsIterator.filter(grid(_) == '#').toSet
      .map(b => b.copy(x = b.x * 2))

  def occupied2(pos: Point, boxes: Set[Point]): Boolean =
    val left = pos.move(Dir.W)
    boxes(pos) || walls(pos) || boxes(left) || walls(left)

  def connectedBoxes(pos: Point, dir: Dir, boxes: Set[Point]): Set[Point] =
    assert(dir.isVertical)
    def search(p: Point): Set[Point] =
      val n = p.move(dir)
      val nw = n.move(Dir.W)
      val ne = n.move(Dir.E)
      if boxes(n) then
        search(n) union search(ne) + n
      else if boxes(nw) then
        search(nw) union search(n) + nw
      else Set.empty[Point]
    search(pos)

  val states = movements.zipWithIndex.scanLeft((startPos, startBoxes)):
    case ((pos, boxes), (dir, i)) =>
      assert(!(boxes(pos) || walls(pos)))
      assert(!(boxes(pos.w) || walls(pos.w)))
      // assert(boxes.map(_.e).intersect(walls).isEmpty, s"box clips walls $i")
      // assert(boxes.intersect(walls.map(_.e)).isEmpty, s"box clips walls $i")
      // assert(boxes.intersect(walls).isEmpty, s"box overlaps walls $i")
      val nextPos = pos.move(dir)
      if !occupied2(nextPos, boxes) then
        nextPos -> boxes
      else if walls(nextPos) || walls(nextPos.move(Dir.W)) then
        pos -> boxes
      else if dir.isVertical then
        val connected = connectedBoxes(pos, dir, boxes)
        assert(connected.nonEmpty, s"no connected boxes: $pos, $dir, ${boxes(nextPos)}")
        val noWalls = connected.forall: b =>
          val n = b.move(dir)
          !(walls(n) || walls(n.e) || walls(n.w))

        if noWalls then
          val nextBs = (boxes -- connected) ++ connected.map(_.move(dir))
          assert(nextBs != boxes)
          // assert(nextBs.intersect(walls).isEmpty, "connected")
          nextPos -> nextBs
        else // walls
          pos -> boxes
      else
        assert(boxes(nextPos) || boxes(nextPos.move(Dir.W)), s"no box in way: ${dir}")
        val farSide = dir match
            case Dir.E =>
              Iterator.iterate(pos.e)(_.e.e).dropWhile(boxes).next
            case Dir.W =>
              Iterator.iterate(pos.w.w)(_.w.w).dropWhile(boxes).next
            case _ => ???

        if walls(farSide) then
          pos -> boxes
        else
          val boxesToMove = dir match
            case Dir.E =>
              Iterator.iterate(pos.e)(_.e.e).takeWhile(boxes).toSet
            case Dir.W =>
              Iterator.iterate(nextPos.w)(_.w.w).takeWhile(boxes).toSet
            case _ => ???

          assert(boxesToMove.nonEmpty, s"horizontal boxes nonempty: pos: $pos, dir: $dir, ${boxes(nextPos)}, ${boxes(nextPos.w)}")
          assert(boxesToMove.map(_.y).sizeIs == 1, "boxes not horizontal")

          val nextBs = (boxes -- boxesToMove) ++ boxesToMove.map(_.move(dir))

          assert(!nextBs.contains(farSide))

          // assert(nextBs.intersect(walls).isEmpty,
          //   s"east west push: intersect: ${nextBs.intersect(walls)} pos: $pos, $dir, tomove: $boxesToMove, ${boxesToMove.map(_.move(dir))} $i")
          assert(nextBs != boxes)

          nextPos -> nextBs

  val (finalPos2, finalBoxes2) = states.last

  val (positions, boxStates) = states.unzip
  assert(boxStates.forall(_.size == startBoxes.size))
  assert(boxStates.distinct.sizeIs > 10)

  val ans2 = finalBoxes2.toList.map:
    case Point(x, y) => 100 * y + x
  .sum

  println(ans2)

  val area2 = Area(
    top = area.top,
    left = area.left,
    right = area.right * 2,
    bot = area.bot
  )

  assert(boxStates.forall(_.size == startBoxes.size))
  assert(boxStates.distinct.sizeIs > 10)
  assert(states.map(_._2).distinct.sizeIs > 10)
  states.zipWithIndex.foreach:
    case ((pos, bs), i) =>
      println(s"$i ${movements(i)}")
      println:
        area2.draw: p =>
          if pos == p then '@'
          else if bs(p) then '['
          else if bs(p.move(Dir.W)) then ']'
          else if walls(p) then '#'
          else if walls(p.move(Dir.W)) then '#'
          else '.'

  println(ans2)

// 1507629 too low
// 1509774 too low

//
