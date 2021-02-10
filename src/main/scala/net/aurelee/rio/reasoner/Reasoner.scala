package net.aurelee.rio.reasoner

import leo.datastructures.TPTP
import net.aurelee.rio.core._
import net.aurelee.rio.reasoner

object Reasoner {
  sealed abstract class RioResult
  case object OutputAccepted extends RioResult
  case object OutputRejected extends RioResult
  case class OutputGenerated(output: Seq[Formula]) extends RioResult
  case class MixedResult(accepted: Seq[Formula], rejected: Seq[Formula]) extends RioResult

  final def apply(problem: Seq[TPTP.AnnotatedFormula])(config: RioConfig): RioResult = {
    import scala.collection.mutable
    // Treat axioms as norm
    // Treat hypotheses as input
    // Treat conjecture as potential output
    val axiomFormulas: mutable.Map[String, Norm] = mutable.Map.empty
    val hypFormulas: mutable.Map[String, Formula] = mutable.Map.empty
    val conjectureFormulas: mutable.Map[String, Formula] = mutable.Map.empty

    problem.foreach {
      case TPTP.THFAnnotated(name, role, TPTP.THF.Logical(formula), _) =>
        role match {
          case "axiom" =>
            if (axiomFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else axiomFormulas += (name -> interpretNorm(formula))
          case "hypothesis" =>
            if (hypFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else hypFormulas += (name -> interpretFormula(formula))
          case "conjecture" =>
            if (conjectureFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else conjectureFormulas += (name -> interpretFormula(formula))
          case _ => throw new UnsupportedOperationException(s"Role '$role' of formula '$name' is not supported.")
        }
      case formula => throw new UnsupportedOperationException(s"Only THF logic formulas are supported, but formula '${formula.name}' is ${formula.formulaType.toString}.")
    }

    // Output is the gross output if no constraints are used. Otherwise handle the constrained case.
    val outputBasis = config.constrained match {
      case Some(netOutputFunction) =>
        val maxFamily = reasoner.maxFamily(config.operator, hypFormulas.values.toVector, axiomFormulas.values.toVector, config.constraints, config.throughput)
        val outFamily = reasoner.outFamily(maxFamily, config.operator, hypFormulas.values.toVector, config.throughput)
        netOutputFunction(outFamily)
      case None => config.operator.apply(axiomFormulas.values.toVector, hypFormulas.values.toVector, config.throughput)
    }

    if (conjectureFormulas.isEmpty) {
      OutputGenerated(outputBasis)
    } else {
      import net.aurelee.rio.sat.consequence
      val (accepted, rejected) = conjectureFormulas.partition(cf => consequence(outputBasis, cf._2))
      if (accepted.isEmpty) OutputRejected
      else if (rejected.isEmpty) OutputAccepted
      else {
        MixedResult(accepted.values.toSeq, rejected.values.toSeq)
      }

    }
  }
}
