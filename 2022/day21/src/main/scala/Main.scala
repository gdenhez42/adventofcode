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

@main def day21: Unit = 
  println(part1 (readInput()))

