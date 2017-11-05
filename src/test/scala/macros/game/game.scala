package macros.game

import scala.collection.immutable.Seq
import Util._

object game {
  val potatoMaster = "potatao"
  val potatoMasfsdter = "potadgatao"
  val potatoadsgMaster = "potatadghao"
  val level = 1
  val money = 0
  val reward = 50
  val objective = "Count amount of even numbers in the input list."

  def evenNumbers(value: Int): Boolean = value % 2 == 0

  def testInput: Seq[Int] = Seq(154, 154, 154, 154, 154, 154, 154, 154, 154, 154)

  def result: Seq[Boolean] = testInput.map(evenNumbers)

  def solutionHash: String = "9804851b616e42359aec4896418d279bf679821d0e919d9d40159488a5a8591a"

  def resultHash: String = "9804851b616e42359aec4896418d279bf679821d0e919d9d40159488a5a8591a"

  def main(args: Array[String]): Unit = {
    println(s"""def resultHash: String = "${Util.hashSeq(result)}"""")
  }
}
