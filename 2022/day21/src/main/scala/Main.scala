import scala.io.Source

abstract class Monkey:
  val name: String
  val t: String

class YellMonkey(val name: String, val value: Int) extends Monkey:
  val t = "yell"
  override def toString(): String = s"Yell $name $value"

class MathMonkey(val name: String, val leftMonkey: String, val op: String, val rightMonkey: String) extends Monkey:
  val t = "math"
  override def toString(): String = s"Math $name $leftMonkey $op $rightMonkey"

def parseMonkey(line: String) : Monkey =
  val tokens = line.split(" ")
  tokens.length match
    case 2 => YellMonkey(tokens(0).substring(0, tokens(0).length - 1), tokens(1).toInt)
    case _ => MathMonkey(tokens(0).substring(0, tokens(0).length - 1), tokens(1), tokens(2), tokens(3))

def readInput(): List[Monkey] =
  val bufferedSource = Source.fromFile("input_test.txt")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close
  lines.map((e: String) => parseMonkey(e))

def getSquareString(input: Double): String =
  val square = input * input
  square.toString

@main def hello: Unit = 
  for (line <- readInput()) {
    println(line)
  }

def msg = "I was compiled by Scala 3. :)"
