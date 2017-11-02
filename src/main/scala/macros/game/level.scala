package macros.game

import scala.collection.immutable.Seq
import scala.annotation._
import scala.meta._

class level extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    def checkLevel(stats: Seq[Stat]): Long = {
      stats.collect {
        //                case v@Defn.Val(_, List(patVar: Pat.Var.Term), _, longValue: Lit.Long) if patVar.....value == "level" => longValue.value
        case _ => 2L
      }.headOption.getOrElse(-1L)
    }

    def checkMoney(stats: Seq[Stat]): Long = {
      stats.collect {
        //        case v@Defn.Val(_, List(patVar: Pat.Var), _, longValue: Lit.Long) if patVar...value == "money" => longValue.value
        case _ => 0L
      }.headOption.getOrElse(-1L)
    }

    defn match {
      case obj@Defn.Object(_, name, template) if name.value == "game" =>
        val level = checkLevel(template.stats.get)
        val money = checkMoney(template.stats.get)

        val updatedStats =
          Seq(
            s"val level = $level".parse[Stat].get,
            s"val money = $money".parse[Stat].get,
          )

        obj.copy(templ = template.copy(stats = Some(updatedStats)))

      //          template.stats.fold(defn) {
      //            objectBody =>
      //              obj.copy(templ = template)
      //          }
      case _ =>
        println(defn.structure)
        abort("@Improved must annotate an object")
    }

  }

}
