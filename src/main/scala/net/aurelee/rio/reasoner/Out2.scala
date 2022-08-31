package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator, PLBottom, PLConj, PLDisj, PLFormula, cnfFormulaToMultiset, cnf, interreduce, mkNeg}


object Out2 extends OutOperator {
  private type CNF = Seq[Clause]
  private type Clause = Seq[Literal]
  private type Literal = Formula
  private type Head = Formula


  override final def name: String = "out2"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) {
      interreduce(materialization(norms).concat(input))
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
        val triggeredCNFs = negatedBodiesCNF.filter( cnf => cnf.forall(x => weaklyTriggeredSet.contains(x)) )
//        println(s"triggeredCNFs = ${triggeredCNFs.toString()}")
        val intermediate = triggeredCNFs.map { cnf =>
          val heads = negatedBodyCNFToHead(cnf)
          heads.reduce(PLConj)
        }
        val weakOutput = intermediate.foldLeft(PLBottom:PLFormula)(PLDisj)
        weakOutput
      }
      interreduce(weakOutputs)
    }
  }
}
