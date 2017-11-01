package macros.game

import scala.annotation._
import scala.meta._

class level extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    def checkMoney(stats: Seq[Stat]): Long = {
      stats.collect {
//        case v@Defn.Val(_, List(patVar: Pat.Var), _, longValue: Lit.Long) if patVar...value == "money" => longValue.value
        case _ => 0L
      }.headOption.getOrElse(-1L)
    }

    defn match {
      case obj@Defn.Object(_, name, template) =>
        if (name.value == "level1") {
          val money = checkMoney(template.stats.get)

          obj.copy(name = name.copy(value = "level2"), templ = template.copy(stats = Some( Nil :+ s"val money = $money".parse[Stat].get)))
        } else {
          //          template.stats.fold(defn) {
          //            objectBody =>
          //              obj.copy(templ = template)
          //          }
          ???
        }
      case _ =>
        println(defn.structure)
        abort("@Improved must annotate an object")
//        ???
        ???
    }

  }

}
