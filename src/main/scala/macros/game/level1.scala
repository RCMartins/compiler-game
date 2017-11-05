package macros.game

import scala.collection.immutable.Seq
import scala.util.Random

object level1 extends LevelDefinition[Int, Boolean] {

  override val objective: String =
    """
      |Count amount of even numbers in the input list.
    """.stripMargin.trim.replaceAll("\\n", "\\\\n")

  override val templateName: String = "evenNumbers"

  override val template: String =
    s"""
       |def $templateName(value: Int): Boolean = ???
    """.stripMargin

  override val reward: Int = 50

  override def generateTest: (Seq[Int], Int => Boolean) = {
    def next = Random.nextInt(1000)
    ((1 to 10).map(_ => next), _ % 2 == 0)
  }

  override def inputType: String = "Int"

  override def outputType: String = "Boolean"

}
