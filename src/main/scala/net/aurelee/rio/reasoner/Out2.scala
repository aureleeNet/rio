package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator, dnf, heads, interreduce, mkConjs, mkDisjs}

object Out2 extends OutOperator {
  override final def name: String = "out2"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) {
      interreduce(materialization(norms).concat(input))
    } else {
      val clauses = dnf(mkConjs(input)).disjs.map(_.conjs)
      val partialResults = clauses.map { cl =>
        val triggered = getBasicTriggeredNorms(cl, norms)
        val triggeredHeads = heads(triggered)
        mkConjs(interreduce(triggeredHeads))
      }
      val bigDisjunction = mkDisjs(partialResults)
      interreduce(Seq(bigDisjunction))
    }
  }
}
