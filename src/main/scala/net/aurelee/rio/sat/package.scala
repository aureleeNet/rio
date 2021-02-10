package net.aurelee.rio

import net.aurelee.rio.core.Formula

package object sat {
  implicit lazy val globalPicoSATInstance: PicoSAT = PicoSAT.get()

  final def cnfToPicoSATClauses(cnf: Formula): Seq[Seq[Int]] = {
    import net.aurelee.rio.core.{PLNeg, PLProp, PLTop, PLBottom}
    import scala.collection.mutable

    if (cnf == PLTop) Seq.empty
    else if (cnf == PLBottom) Seq(Seq.empty)
    else {
      val result: mutable.ListBuffer[Seq[Int]] = mutable.ListBuffer.empty
      val clauses = cnf.conjs
      val symbols = cnf.symbols.toVector
      clauses.foreach { cl =>
        val lits = cl.disjs
        val translatedLits = lits.map { l => (l: @unchecked) match {
          case PLNeg(PLProp(name)) => -(symbols.indexOf(name)+1)
          case PLProp(name) => symbols.indexOf(name)+1
        }
        }
        result.append(translatedLits)
      }
      result.toSeq
    }
  }


  final def satisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.SAT
  }
  final def unsatisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.UNSAT
  }
  final def consistent(formulas: Iterable[Formula])(implicit solver: PicoSAT): Boolean = {
    import net.aurelee.rio.core.mkConjs
    val conjunct = mkConjs(formulas)
    satisfiable(conjunct)(solver)
  }
  final def consequence(assumptions: Iterable[Formula], conjecture: Formula)(implicit solver: PicoSAT): Boolean = {
    import net.aurelee.rio.core.{mkConjs, mkNeg}
    val formulas = Iterable.concat(assumptions, Seq(mkNeg(conjecture)))
    unsatisfiable(mkConjs(formulas))(solver)
  }
  final def tautology(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    import net.aurelee.rio.core.mkNeg
    unsatisfiable(mkNeg(formula))(solver)
  }
  final def equivalent(f: Formula, g: Formula)(implicit solver: PicoSAT): Boolean = {
    import net.aurelee.rio.core.{mkImpl, mkConjs}
    val impl = mkImpl(f, g)
    val lpmi = mkImpl(g, f)
    tautology(mkConjs(Seq(impl, lpmi)))(solver)
  }
  private[this] final def solveInternal(formula: Formula)(solver: PicoSAT): Unit = {
    import net.aurelee.rio.core.{cnf, simp}
    val clausified = cnf(formula)
    val simplified = simp(clausified)
//    println(s"[PicoSAT] Check ${simplified.pretty}")
    val picoSatInput = cnfToPicoSATClauses(simplified)
    solver.reset()
    picoSatInput.foreach(solver.addClause)
    solver.solve()
  }


}
