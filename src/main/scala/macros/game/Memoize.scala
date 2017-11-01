package macros.game

import scala.annotation._
import scala.meta._

@compileTimeOnly("Should be used only in compile time.")
class Memoize extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case _ =>
        defn
        //abort("@Memoize must annotate an function.")
    }
  }
}
