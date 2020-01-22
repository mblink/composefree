package composefree

import scala.reflect.internal.util.TransparentPosition
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

class ComposeFreePlugin(override val global: Global) extends Plugin { self =>
  import global._

  private case class PrettyPrinter(level: Int, inQuotes: Boolean, backslashed: Boolean) {
    val indent = List.fill(level)("  ").mkString

    def transform(char: Char): (PrettyPrinter, String) = {
      val woSlash = copy(backslashed = false)
      val (pp, f): (PrettyPrinter, PrettyPrinter => String) = char match {
        case '"' if inQuotes && !backslashed => (woSlash.copy(inQuotes = false), _ => s"$char")
        case '"' if !inQuotes => (woSlash.copy(inQuotes = true), _ => s"$char")
        case '\\' if inQuotes && !backslashed => (copy(backslashed = true), _ => s"$char")

        case ',' if !inQuotes => (woSlash, p => s",\n${p.indent}")
        case '(' if !inQuotes => (woSlash.copy(level = level + 1), p => s"(\n${p.indent}")
        case ')' if !inQuotes => (woSlash.copy(level = level - 1), p => s"\n${p.indent})")
        case _ => (woSlash, _ => s"$char")
      }
      (pp, f(pp))
    }
  }

  private def prettyPrint(raw: String): String =
    raw.foldLeft((PrettyPrinter(0, false, false), new StringBuilder(""))) { case ((pp, sb), char) =>
      val (newPP, res) = pp.transform(char)
      (newPP, sb.append(res))
    }._2.toString.replaceAll("""\(\s+\)""", "()")

  private def showTree(tree: Tree, pretty: Boolean): String =
    if (pretty) prettyPrint(showRaw(tree)) else showRaw(tree)

  private def debugStr(name: String, tree: Tree, pretty: Boolean = true): String =
    s"===\n$name ${tree.pos}:\n${show(tree)}\n${showTree(tree, pretty)}"

  protected def debug(args: (String, Any)*): Unit = println(s"""
    |********************************************************************************
    |${args.map { case (s, v) => if (v.isInstanceOf[Tree]) debugStr(s, v.asInstanceOf[Tree]) else s"$s: $v" }.mkString("\n\n")}
    |********************************************************************************
    |""".stripMargin)

  protected def debug(name: String, tree: Tree, pretty: Boolean = true): Unit =
    println(debugStr(name, tree, pretty))

  override lazy val name: String = "composefree"
  override lazy val description: String = "Expands composefree syntax"

  def freshName(prefix: String): String = currentFreshNameCreator.newName(prefix)

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = ComposeFreePlugin.this.name
    override val global: ComposeFreePlugin.this.global.type =
      ComposeFreePlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = newTransformer(unit).transformUnit(unit)
    }

    private def newTransformer(unit: CompilationUnit) =
      new TypingTransformer(unit) {
        override def transform(tree: Tree): Tree = super.transform(tree match {
          case Apply(TypeApply(Ident(TermName("composed")), tpes), args) =>
            val valName = TermName(freshName("composefree"))
            withAllPos(q"""
            val ${valName} = _root_.composefree.ComposeFree[..$tpes]
            import ${Ident(valName)}._
            ..$args
            """, tree.pos)
          case _ => tree
        })
    }

    override val runsAfter: List[String] = List("parser")
    override val runsBefore: List[String] = List("namer")

    private def withAllPos[A <: Tree](tree: A, pos: Position): A = {
      tree.foreach { t =>
        if (!t.pos.isDefined || t.pos == NoPosition)
          t.setPos(new TransparentPosition(pos.source, pos.start, pos.end, pos.end))
        ()
      }
      tree
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
