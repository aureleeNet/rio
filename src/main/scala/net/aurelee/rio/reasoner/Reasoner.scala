package net.aurelee.rio.reasoner

import leo.datastructures.TPTP
import net.aurelee.rio.core._
import net.aurelee.rio.{SemanticsException, reasoner}

object Reasoner {
  sealed abstract class RioResult
  case class OutputGenerated(output: Seq[Formula]) extends RioResult
  case class OutputVerified(accepted: Seq[(String, Formula)], rejected: Seq[(String, Formula)]) extends RioResult

  final def apply(problem: Seq[TPTP.AnnotatedFormula])(config: RioConfig): RioResult = {
    println(s"% Use configuration: ${config.pretty}")
    import scala.collection.mutable
    // Treat axioms as norm
    // Treat hypotheses as input
    // Treat conjecture as potential output
    val axiomFormulas: mutable.Map[String, Norm] = mutable.Map.empty
    val hypFormulas: mutable.Map[String, Formula] = mutable.Map.empty
    val conjectureFormulas: mutable.Map[String, Formula] = mutable.Map.empty

    problem.foreach {
      case TPTP.TFFAnnotated(name, role, TPTP.TFF.Logical(formula), _) =>
        role match {
          case "axiom" =>
            if (axiomFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else axiomFormulas += (name -> interpretNorm(formula))
          case "hypothesis" =>
            if (hypFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else hypFormulas += (name -> interpretTFFFormula(formula))
          case "conjecture" =>
            if (conjectureFormulas.isDefinedAt(name)) throw new UnsupportedOperationException(s"Formula '$name' defined more than once.")
            else conjectureFormulas += (name -> interpretTFFFormula(formula))
          case _ => throw new UnsupportedOperationException(s"Role '$role' of formula '$name' is not supported.")
        }
      case formula => throw new UnsupportedOperationException(s"Only TFF logic formulas are supported, but formula '${formula.name}' is ${formula.formulaType.toString}.")
    }
    val preferenceRelation: Option[PreferenceRelation[Norm]] = config.preferenceRelation.map { list =>
      val namesToNorms: Seq[Seq[Norm]] = list.map { level =>
        level.map { normName => axiomFormulas.getOrElse(normName, throw new SemanticsException(s"Unknown norm '$normName' in preference declaration."))}
      }
      PreferenceRelation.fromSeqs(namesToNorms)
    }

    // Output is the gross output if no constraints are used. Otherwise handle the constrained case.
    val outputBasis = config.constrained match {
      case Some(netOutputFunction) =>
        val constraints = config.constraints match {
          case Seq(PLProp("$$input")) => hypFormulas.values.toSeq
          case _ => config.constraints
        }
//        val maxFamily = reasoner.maxFamily(config.operator, hypFormulas.values.toVector, axiomFormulas.values.toVector, constraints, config.throughput)
        val maxFamily = reasoner.maxFamilyWithNames(config.operator, hypFormulas.toMap, axiomFormulas.toMap, constraints, config.throughput)
        println(s"% Info: maxFamily = ${maxFamily.map(x => x.keys.mkString("{", ",", "}")).mkString("[", ", ", "]")}")
        val maxFamilyAsFormulas = maxFamily.map(_.values.toSeq)
        val selectedSubsetOfMaxFamily = preferenceRelation match {
          case Some(rel) =>
            val liftedRelation = PreferenceRelation.brassLifting(rel)
            PreferenceRelation.getMaximals(liftedRelation, maxFamilyAsFormulas)
          case None => maxFamilyAsFormulas
        }
        println(s"% Info: prefFamily = ${selectedSubsetOfMaxFamily.map(x => x.map(prettyNorm).mkString("(", ",", ")")).mkString("[", ", ", "]")}")
        val outFamily = reasoner.outFamily(selectedSubsetOfMaxFamily, config.operator, hypFormulas.values.toVector, config.throughput)
//        println(s"outFamily = ${outFamily.map(x => x.map(_.pretty).mkString("[", ",", "]")).mkString("{", ",\n", "}")}")
        netOutputFunction(outFamily)
      case None => config.operator.apply(axiomFormulas.values.toVector, hypFormulas.values.toVector, config.throughput)
    }

    if (conjectureFormulas.isEmpty) {
      OutputGenerated(outputBasis)
    } else {
      import net.aurelee.rio.sat.consequence
      val (accepted, rejected) = conjectureFormulas.partition(cf => consequence(outputBasis, cf._2))
      OutputVerified(accepted.toSeq, rejected.toSeq)
    }
  }
}
