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
//        abort(defn.structure)

        val level = checkLevel(template.stats.get)
        val money = checkMoney(template.stats.get)

        val updatedMods = //Nil :+ Mod.Annot.apply("level".parse[Term].get)
//        Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name("level"), Nil)))
        Seq(Mod.Annot(Ctor.Ref.Name("fight")), Mod.Annot(Ctor.Ref.Name("level")))

        val updatedStats =
          Seq(
            s"val level = ${level + 1}".parse[Stat].get,
            s"val money = ${money + 50}".parse[Stat].get,
          )

//        q"""
//           ..$updatedMods
//           object game {
//             ..$updatedStats
//           }
//         """
        obj.copy(mods = updatedMods, templ = template.copy(stats = Some(updatedStats)))
      case _ =>
        abort(defn.structure)
        abort("@Improved must annotate an object")
    }

  }

}

class fight extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn
  }

}