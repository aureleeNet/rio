package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator, heads}

object Out3 extends OutOperator {
  override final def name: String = "out3"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    var facts = input
    var triggeredNorms = getDirectlyTriggeredNorms(input, norms)
    var passiveNorms = norms.diff(triggeredNorms)
    var newNorms = triggeredNorms
    while (newNorms.nonEmpty) {
      facts = facts.concat(heads(triggeredNorms))
      newNorms = getDirectlyTriggeredNorms(facts, passiveNorms)
      triggeredNorms = triggeredNorms.concat(newNorms)
      passiveNorms = passiveNorms.diff(newNorms)
    }
    if (throughput) heads(triggeredNorms).concat(input)
    else heads(triggeredNorms)
  }
}
