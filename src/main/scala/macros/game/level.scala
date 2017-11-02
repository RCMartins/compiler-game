package macros.game

import scala.collection.immutable.Seq
import scala.annotation._
import scala.meta._

class level extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    def checkLevel(stats: Seq[Stat]): Int = {
      stats.collect {
        case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, number: Lit.Int) if patVar.name.value == "level" => number.value
      }.headOption.getOrElse(-1)
    }

    def checkMoney(stats: Seq[Stat]): Int = {
      stats.collect {
        case Defn.Val(_, Seq(patVar: Pat.Var.Term), _, number: Lit.Int) if patVar.name.value == "money" => number.value
      }.headOption.getOrElse(-1)
    }

    defn match {
      case obj@Defn.Object(_, name, template) if name.value == "game" =>
        val level = checkLevel(template.stats.get)
        val money = checkMoney(template.stats.get)

        val updatedStats =
          Seq(
            s"val level = ${level + 1}".parse[Stat].get,
            s"val money = ${money + 50}".parse[Stat].get,
          )

        obj.copy(templ = template.copy(stats = Some(updatedStats)))
      case _ =>
        println(defn.structure)
        abort("@Improved must annotate an object")
    }

  }

}
