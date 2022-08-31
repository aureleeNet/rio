package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator, PLBottom, PLConj, PLDisj, PLFormula, PLNeg, bodies, cnf, dnf, heads, interreduce, mkConjs, mkDisjs, mkNeg, simp}


object Out2 extends OutOperator {
  private type CNF = Seq[Clause]
  private type Clause = Seq[Literal]
  private type Literal = Formula
  private type Body = Formula
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
        val simpCNFOfNegatedBody: CNF = simp(cnf(mkNeg(body))).conjs.map(_.disjs)
        if (negatedBodyCNFToHead.isDefinedAt(simpCNFOfNegatedBody)) {
          val value = negatedBodyCNFToHead(simpCNFOfNegatedBody)
          negatedBodyCNFToHead += (simpCNFOfNegatedBody -> (value :+ head))
        } else {
          negatedBodyCNFToHead += (simpCNFOfNegatedBody -> Seq(head))
        }
      }
//      val negatedBodyCNFToHead: Map[CNF, Seq[Head]] = ???
      val negatedBodiesCNF: Seq[CNF] = negatedBodyCNFToHead.keys.toSeq
      val minimallyWeaklyTriggeredSets: Seq[CNF] = getMinimallyWeaklyTriggeredSets(input, negatedBodiesCNF)
      val weakOutputs = minimallyWeaklyTriggeredSets.map { mus =>
        val triggeredCNFs = negatedBodiesCNF.filter( cnf => cnf.forall(mus.contains) )
        val intermediate = triggeredCNFs.map { cnf =>
          val heads = negatedBodyCNFToHead(cnf)
          heads.reduce(PLConj)
        }
        val weakOutput = intermediate.foldLeft(PLBottom:PLFormula)(PLDisj)
        weakOutput
      }
      interreduce(weakOutputs)

//      val normBodies = bodies(norms)
//      val negatedNormBodies: Seq[Formula] = normBodies.map(PLNeg)
//      val clauseSetForMUSEnumeration = input.concat(negatedNormBodies)
//
//      val clauses = dnf(mkConjs(input)).disjs.map(_.conjs)
//      val partialResults = clauses.map { cl =>
//        val triggered = getBasicTriggeredNorms(cl, norms)
//        val triggeredHeads = heads(triggered)
//        mkConjs(interreduce(triggeredHeads))
//      }
//      val bigDisjunction = mkDisjs(partialResults)
//      interreduce(Seq(bigDisjunction))
    }
  }
}
