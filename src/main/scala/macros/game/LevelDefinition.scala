package macros.game

import scala.collection.immutable.Seq

trait LevelDefinition[A, B] {

  def objective: String

  def templateName: String

  def template: String

  def reward: Int

  def generateTest: (Seq[A], A => B)

  def inputType: String

  def outputType: String

  def testType: String = s"$inputType => $outputType"

}
