package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{CNF, Formula, Norm, Head}
import net.aurelee.rio.core.OutOperator
import net.aurelee.rio.core.{cnf, cnfFormulaToMultiset, mkConjs, mkDisjs, mkNeg, heads}


object Out2 extends OutOperator {
  override final def name: String = "out2"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) {
      materialization(norms).concat(input)
    } else {
      import scala.collection.mutable
      // Transform list of norms to Map CNF[negated body] -> Heads
      val negatedBodyCNFToHead: mutable.Map[CNF, Seq[Head]] = mutable.Map.empty
      norms.foreach { case (body, head) =>
        val simpCNFOfNegatedBody: CNF = cnfFormulaToMultiset(cnf(mkNeg(body)))
        if (simpCNFOfNegatedBody == Seq.empty) { /* skip */ } // $true can never be part of a MUS
        else {
          if (negatedBodyCNFToHead.isDefinedAt(simpCNFOfNegatedBody)) {
            val value = negatedBodyCNFToHead(simpCNFOfNegatedBody)
            negatedBodyCNFToHead += (simpCNFOfNegatedBody -> (value :+ head))
          } else {
            negatedBodyCNFToHead += (simpCNFOfNegatedBody -> Seq(head))
          }
        }
      }
//      println(s"negatedBodyCNFToHead = ${negatedBodyCNFToHead.toString()}")
      val negatedBodiesCNF: Seq[CNF] = negatedBodyCNFToHead.keys.toSeq
//      println(s"negatedBodiesCNF = ${negatedBodiesCNF.toString()}")
      val minimallyWeaklyTriggeredSets: Seq[CNF] = getMinimallyWeaklyTriggeredSets(input, negatedBodiesCNF)
      val weakOutputs = minimallyWeaklyTriggeredSets.map { weaklyTriggeredSet =>
//        println(s" weaklyTriggeredSet = ${weaklyTriggeredSet.toString()}")
        if (weaklyTriggeredSet.isEmpty) {
          // then the input is inconsistent and everything is triggered
          mkConjs(heads(norms))
        } else {
          // collect heads from weakly triggered norms
          val triggeredCNFs = negatedBodiesCNF.filter(cnf => cnf.forall(x => weaklyTriggeredSet.contains(x)))
          //        println(s"triggeredCNFs = ${triggeredCNFs.toString()}")
          val weaklyTriggeredNormHeads = triggeredCNFs.map { cnf =>
            val heads = negatedBodyCNFToHead(cnf)
            mkConjs(heads)
          }
          val weakOutput = mkDisjs(weaklyTriggeredNormHeads)
          weakOutput
        }
      }
      weakOutputs
    }
  }
}
