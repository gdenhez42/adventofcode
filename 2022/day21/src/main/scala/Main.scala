import scala.io.Source

abstract class Monkey:
  val name: String
  val t: String

class YellMonkey(val name: String, val value: Long) extends Monkey:
  val t = "yell"
  override def toString(): String = s"Yell $name $value"

class MathMonkey(val name: String, val leftMonkey: String, val op: String, val rightMonkey: String) extends Monkey:
  val t = "math"
  override def toString(): String = s"Math $name $leftMonkey $op $rightMonkey"

def parseMonkey(line: String) : Monkey =
  val tokens = line.split(" ")
  tokens.length match
    case 2 => YellMonkey(tokens(0).substring(0, tokens(0).length - 1), tokens(1).toLong)
    case _ => MathMonkey(tokens(0).substring(0, tokens(0).length - 1), tokens(1), tokens(2), tokens(3))

def readInput(): List[Monkey] =
  val bufferedSource = Source.fromFile("input.txt")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close
  lines.map((e: String) => parseMonkey(e))

def doSomeMath(v1: Long, v2: Long, op: String) : Long =
  op match
    case "+" => v1 + v2
    case "-" => v1 - v2
    case "*" => v1 * v2
    case "/" => v1 / v2

def solveLeft(v2: Long, result: Long, op: String) : Long =
  op match
    case "+" => result - v2
    case "-" => result + v2
    case "*" => result / v2
    case "/" => result * v2

def solveRight(v1: Long, result: Long, op: String) : Long =
  op match
    case "+" => result - v1
    case "-" => v1 - result
    case "*" => result / v1
    case "/" => v1 / result


def part1 (monkeys : List[Monkey]) : Long =
  val vals = scala.collection.mutable.Map[String, Long]()
  for (monkey <- monkeys) {
    if (monkey.t == "yell") vals(monkey.name) = monkey.asInstanceOf[YellMonkey].value
  }
  var found = true
  while (found) {
    found = false

    for (monkey <- monkeys) {
      monkey match
        case math: MathMonkey =>
          (vals.get(math.name), vals.get(math.leftMonkey), vals.get(math.rightMonkey)) match
            case (None, Some(v1), Some(v2)) =>
              vals(math.name) = doSomeMath(v1, v2, math.op)
              found = true
            case _ => ()
        case _ => ()
    }
  }
  vals.get("root").get

def part2 (monkeys : List[Monkey]) : Long =
  val vals = scala.collection.mutable.Map[String, Long]()
  val mathMonkeys = scala.collection.mutable.Map[String, MathMonkey]()
  for (monkey <- monkeys) {
    if (monkey.t == "yell" && monkey.name != "humn") vals(monkey.name) = monkey.asInstanceOf[YellMonkey].value
    if (monkey.t == "math") mathMonkeys(monkey.name) = monkey.asInstanceOf[MathMonkey]
  }
  var foundVals = true
  while (foundVals) {
    foundVals = false

    for (monkey <- monkeys) {
      monkey match
        case math: MathMonkey =>
          (vals.get(math.name), vals.get(math.leftMonkey), vals.get(math.rightMonkey)) match
            case (None, Some(v1), Some(v2)) =>
              vals(math.name) = doSomeMath(v1, v2, math.op)
              foundVals = true
            case _ => ()
        case _ => ()
    }
  }

  var foundHumn = false
  var result = 0L
  var currentMonkey = mathMonkeys.get("root").get
  (vals.get(currentMonkey.leftMonkey), vals.get(currentMonkey.rightMonkey)) match
    case (None, Some(v)) =>
      vals(currentMonkey.leftMonkey) = v
      result = v
      if currentMonkey.leftMonkey == "humn" then
        foundHumn = true
      else
        currentMonkey = mathMonkeys.get(currentMonkey.leftMonkey).get
    case (Some(v), None) =>
      vals(currentMonkey.rightMonkey) = v
      result = v
      if currentMonkey.rightMonkey == "humn" then
        foundHumn = true
      else
        currentMonkey = mathMonkeys.get(currentMonkey.rightMonkey).get
    case _ =>
      throw new RuntimeException("Need fancier solution for monkey " + currentMonkey.name)


  while (!foundHumn) {
    (vals.get(currentMonkey.leftMonkey), vals.get(currentMonkey.rightMonkey)) match
    case (None, Some(v)) =>
      result = solveLeft(v, result, currentMonkey.op)
      vals(currentMonkey.leftMonkey) = result
      if currentMonkey.leftMonkey == "humn" then
        foundHumn = true
      else
        currentMonkey = mathMonkeys.get(currentMonkey.leftMonkey).get
    case (Some(v), None) =>
      result = solveRight(v, result, currentMonkey.op)
      vals(currentMonkey.rightMonkey) = result
      if currentMonkey.rightMonkey == "humn" then
        foundHumn = true
      else
        currentMonkey = mathMonkeys.get(currentMonkey.rightMonkey).get
    case _ =>
      throw new RuntimeException("Need fancier solution for monkey " + currentMonkey.name)
  }

  vals.get("humn").get

@main def day21: Unit =
  val input = readInput()
  println("Part1: " + (part1(input)))
  println("Part2: " + (part2(input)))

