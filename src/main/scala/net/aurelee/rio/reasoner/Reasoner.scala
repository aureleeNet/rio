package net.aurelee.rio.reasoner

import leo.datastructures.TPTP
import net.aurelee.rio.core._

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
            axiomFormulas += (name -> interpretNorm(formula))
          case "hypothesis" =>
            hypFormulas += (name -> interpretFormula(formula))
          case "conjecture" =>
            conjectureFormulas += (name -> interpretFormula(formula))
          case _ => throw new UnsupportedOperationException(s"Role '$role' of formula '$name' is not supported.")
        }
      case formula => throw new UnsupportedOperationException(s"Only THF logic formulas are supported, but formula '${formula.name}' is ${formula.formulaType.toString}.")
    }

//    axiomFormulas.foreach {case (name, f) =>
//      println(s"$name: ${prettyNorm(f)}")
//    }
//    hypFormulas.foreach { case (name, f) =>
//      println(s"$name: ${f.pretty}")
//    }

    val grossOutputBasis = config.operator.apply(axiomFormulas.values.toVector, hypFormulas.values.toVector, config.throughput)
//    grossOutputBasis.foreach { f =>
//      println(f.pretty)
//    }

    val outputBasis = grossOutputBasis
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
