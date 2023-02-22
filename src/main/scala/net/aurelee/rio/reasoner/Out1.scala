package net.aurelee.rio.reasoner

import net.aurelee.rio.core._

object Out1 extends OutOperator {
  override final def name: String = "out1"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    val triggered = getDirectlyTriggeredNorms(input, norms)
    val triggeredHeads = heads(triggered)
    if (throughput) triggeredHeads.concat(input)
    else triggeredHeads
  }
}
