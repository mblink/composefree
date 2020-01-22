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

  type Id[A] = A

  private def freshName(prefix: String): String = currentFreshNameCreator.newName(prefix)

  private def withAllPos(tree: Tree, pos: Position): Tree = {
    tree.foreach { t =>
      if (!t.pos.isDefined || t.pos == NoPosition)
        t.setPos(new TransparentPosition(pos.source, pos.start, pos.end, pos.end))
    }
    tree
  }

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = ComposeFreePlugin.this.name
    override val global: ComposeFreePlugin.this.global.type =
      ComposeFreePlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = newTransformer(unit).transformUnit(unit)
    }

    private def newTransformer(unit: CompilationUnit) =
      new TypingTransformer(unit) {
        private def combineDSL(cbc: Tree, t1: Tree, t2: Tree): Tree = tq"$cbc[$t1, $t2]"

        private def maybeAddNilDSL(tree: Tree): Tree =
          tree match {
            case tq"NilDSL" | tq"freek.NilDSL" | tq"_root_.freek.NilDSL" => tree

            case AppliedTypeTree(t @ (tq":|:" | tq"freek.:|:" | tq"_root_.freek.:|:"), List(t1, t2)) =>
              combineDSL(t, t1, maybeAddNilDSL(t2))

            case t =>
              combineDSL(tq"_root_.freek.:|:", t, tq"_root_.freek.NilDSL")
          }

        private def withComposeFree[F[_]](tpe: Tree)(f: (ValDef, Import) => F[Tree]): F[Tree] = {
          val valName = TermName(freshName("$composefree$"))
          f(q"val ${valName} = _root_.composefree.ComposeFree[${maybeAddNilDSL(tpe)}]",
            q"import ${Ident(valName)}._")
        }

        private def transformApplys(tree: Tree): Tree =
          tree match {
            case Apply(TypeApply(Ident(TermName("Composed")), List(tpe)), List(x)) =>
              withComposeFree[Id](tpe)((v, i) => withAllPos(q"""
                $v
                $i
                $x
              """, tree.pos))

            case Apply(TypeApply(Ident(TermName("Composed")), List(dslTpe, outTpe)), List(x)) =>
              withComposeFree[Id](dslTpe)((v, i) => withAllPos(q"""
                $v
                $i
                $x: _root_.composefree.Composed[${Ident(v.name)}.M, $outTpe]
              """, tree.pos))

            case _ => tree
          }

        private lazy val Composed = TypeName("Composed")
        private lazy val ComposedAp = TypeName("ComposedAp")

        private def transformComposed(cTpe: TypeName, dslTpe: Tree, outTpe: Tree)(mkDefn: (Tree, Import) => Tree): List[Tree] =
          withComposeFree(dslTpe)((v, i) => List(v, mkDefn(tq"_root_.composefree.${cTpe}[${Ident(v.name)}.M, $outTpe]", i)))

        private def transformComposedDefDef(d: DefDef, cTpe: TypeName, dslTpe: Tree, outTpe: Tree): List[Tree] =
          transformComposed(cTpe, dslTpe, outTpe)(
            (t, i) => q"${d.mods} def ${d.name}[..${d.tparams}](...${d.vparamss}): $t = { $i; ${d.rhs} }")

        private def transformComposedValDef(v: ValDef, cTpe: TypeName, dslTpe: Tree, outTpe: Tree): List[Tree] =
          transformComposed(cTpe, dslTpe, outTpe)((t, i) => q"${v.mods} val ${v.name}: $t = { $i; ${v.rhs} }")

        private def selectedFromComposefree(tpe: Tree, cTpe: TypeName): Boolean =
          tpe match {
            case Ident(`cTpe`) => true
            case Select(Ident(TermName("composefree")), `cTpe`) => true
            case Select(Select(Ident(nme.ROOTPKG), TermName("composefree")), `cTpe`) => true
            case _ => false
          }

        private def maybeTransformDefns(tree: Tree, cTpe: TypeName): Option[List[Tree]] =
          tree match {
            case t @ DefDef(_, _, _, _, AppliedTypeTree(tpe, List(d, o)), _) if selectedFromComposefree(tpe, cTpe) =>
              Some(transformComposedDefDef(t, cTpe, d, o))

            case t @ ValDef(_, _, AppliedTypeTree(tpe, List(d, o)), _) if selectedFromComposefree(tpe, cTpe) =>
              Some(transformComposedValDef(t, cTpe, d, o))

            case _ => None
          }

        private def transformDefns(tree: Tree): List[Tree] =
          maybeTransformDefns(tree, Composed)
            .orElse(maybeTransformDefns(tree, ComposedAp))
            .getOrElse(List(transformApplys(tree)))

        override def transform(tree: Tree): Tree =
          super.transform(tree match {
            case p: PackageDef =>
              treeCopy.PackageDef(p, p.pid, p.stats.flatMap(transformDefns))

            case m: ModuleDef =>
              treeCopy.ModuleDef(m, m.mods, m.name,
                treeCopy.Template(m.impl, m.impl.parents, m.impl.self, m.impl.body.flatMap(transformDefns)))

            case c: ClassDef =>
              treeCopy.ClassDef(c, c.mods, c.name, c.tparams,
                treeCopy.Template(c.impl, c.impl.parents, c.impl.self, c.impl.body.flatMap(transformDefns)))

            case _ => transformApplys(tree)
          })
    }

    override val runsAfter: List[String] = List("parser")
    override val runsBefore: List[String] = List("namer")


  }

  override lazy val components: List[PluginComponent] = List(phase)
}
