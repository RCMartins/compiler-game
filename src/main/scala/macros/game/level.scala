package macros.game

import scala.collection.immutable.Seq
import scala.annotation._
import scala.meta._
import org.scalameta.logger
import scala.meta.contrib._
import Util._

class next extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    getObject(defn) match {
      case obj@Defn.Object(_, name, template) if name.value == "game" =>
        val stats = template.stats.get
        val hash = getString(stats, "hash")
        val level = getInt(stats, "level")
        val money = getInt(stats, "money")

        val updatedMods =
          Seq(Mod.Annot(Ctor.Ref.Name("generate")))

        val levelDef = if (level == 1) level1 else level1

        val updatedStatsStr =
          Seq(
            s"""val objective = "${levelDef.objective}"""",
            s"val reward = ${levelDef.reward}",
            s"${levelDef.template}",
          )
        val updatedStats = updatedStatsStr.map(_.parse[Stat].get)
        val updatedNames = getDefNames(updatedStats).map("\"" + _ + "\"").toSet

        obj.copy(mods = updatedMods, templ = template.copy(stats = Some(removeStats(stats, updatedNames) ++ updatedStats)))
    }
  }
}

class generate extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    getObject(defn) match {
      case obj@Defn.Object(_, name, template) if name.value == "game" =>
        val stats = template.stats.get
        val hash = getString(stats, "hash")
        val level = getInt(stats, "level")
        val money = getInt(stats, "money")
        val reward = getInt(stats, "reward")

        val updatedMods =
          Seq()

        val levelDef = level1
        val function = getTemplate(stats, levelDef.templateName)

        val (input, test) = levelDef.generateTest
        val inputFormatted = input.map(format(_)).mkString("Seq(", ", ", ")")
        val result = input.map(test)
        val resultHash = Util.hashSeq(result)

        val triple = "\"\"\""

        val updatedStatsStr =
          Seq(
            s"val level = $level",
            s"val money = $money",
            s"val reward = $reward",
            s"""val objective = "${levelDef.objective}"""",
            s"$function",
            s"def testInput: Seq[${levelDef.inputType}] = $inputFormatted",
            s"def result: Seq[${levelDef.outputType}] = testInput.map(${levelDef.templateName})",
            s"""def solutionHash: String = "$resultHash"""",
            s"""def resultHash: String = ???""",
            s"""def main(args: Array[String]): Unit = { println(s${triple}def resultHash: String = "$${Util.hashSeq(result)}"$triple) }""",
          )
        val updatedStats = updatedStatsStr.map(_.parse[Stat].get)
        val updatedNames = getDefNames(updatedStats).toSet

        logger.debug(updatedNames)
        logger.debug(getDefNames(stats).map("\"" + _ + "\"").toSet)
        logger.debug(getDefNames(removeStats(stats, updatedNames)).map("\"" + _ + "\"").toSet)

        obj.copy(mods = updatedMods, templ = template.copy(stats = Some(removeStats(stats, updatedNames) ++ updatedStats)))
    }
  }
}

//class check extends StaticAnnotation {
//
//  inline def apply(defn: Any): Any = meta {
//    getObject(defn) match {
//      case obj@Defn.Object(_, name, template) if name.value == "game" =>
//        val hash = getString(template.stats.get, "hash")
//        val level = getInt(template.stats.get, "level")
//        val money = getInt(template.stats.get, "money")
//        val reward = getInt(template.stats.get, "reward")
//
//        val updatedMods =
//          Seq()
//
//        val levelDef = level1
//
//        val function = getTemplate(template.stats.get, levelDef.templateName)
//
//        val updatedStats =
//          Seq(
//            s"val level = $level".parse[Stat].get,
//            s"val money = $money".parse[Stat].get,
//            s"val reward = $reward".parse[Stat].get,
//            s"""val objective = "${levelDef.objective}"""".parse[Stat].get,
//            s"$function".parse[Stat].get,
//            s"def testInput = $inputFormatted".parse[Stat].get,
//            s"def solution: ${levelDef.testType} = $test".parse[Stat].get,
//            s"def checkSolution: Boolean = testInput.forall { input => ${levelDef.templateName}(input) == solution(input) }".parse[Stat].get,
//            s"def def main(args: Array[String]): Unit = { println(checkSolution) }".parse[Stat].get,
//          )
//
//        obj.copy(mods = updatedMods, templ = template.copy(stats = Some(updatedStats)))
//    }
//  }
//
//}

object Util {
  def getString(stats: Seq[Stat], varName: String): String = {
    stats.collect {
      case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, str: Lit.String) if patVar.name.value == varName => str.value
    }.headOption.getOrElse("")
  }

  def getInt(stats: Seq[Stat], varName: String): Int = {
    stats.collect {
      case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, number: Lit.Int) if patVar.name.value == varName => number.value
    }.headOption.getOrElse(-1)
  }

  def getTemplate(stats: Seq[Stat], varName: String): Defn.Def = {
    stats.collect {
      case fun@Defn.Def(_, name, _, _, _, term) if name.value == varName => fun
    }.headOption.getOrElse(throw new Exception(s"no function $varName found"))
  }

  def getDefName(stat: Stat): Option[String] = {
    stat match {
      case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, _) => Some(patVar.name.value)
      case Defn.Def(_, name, _, _, _, _) => Some(name.value)
      case _ => None
    }
  }

  def getDefNames(stats: Seq[Stat]): Seq[String] = {
    stats.collect {
      case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, _) => patVar.name.value
      case Defn.Def(_, name, _, _, _, _) => name.value
    }
  }

  def getObject(defn: Any): Defn.Object = {
    defn match {
      case obj: Defn.Object => obj
      case _ =>
        abort("@Improved must annotate an object")
    }
  }

  def removeStats(stats: Seq[Stat], namesToRemove: Set[String]): Seq[Stat] = {
    stats.filterNot {
      stat => getDefName(stat).exists(namesToRemove.contains)
    }
  }

  // sha256
  def hash(text: String): String = {
    String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))))
  }

  trait FotmatForHashing[A] {
    def toHashReady(data: A): String
  }

  implicit val formatString: FotmatForHashing[String] = (data: String) => data
  implicit val formatInt: FotmatForHashing[Int] = (data: Int) => data.toString
  implicit val formatBoolean: FotmatForHashing[Boolean] = (data: Boolean) => data.toString

  def format[A](data: A)(implicit format: FotmatForHashing[A]): String = format.toHashReady(data)

  def hashSeq[A](seq: Seq[A])(implicit format: FotmatForHashing[A]): String = {
    seq.map(format.toHashReady).foldLeft("") { (a, b) => hash(a + b) }
  }

  def checkSolution[A](solutionHash: String, seq: Seq[A])(implicit format: FotmatForHashing[A]): Boolean = {
    solutionHash ==
  }
}