package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator}

object Out4 extends OutOperator {
  override final def name: String = "out4"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) Out2.apply(norms, input, throughput)
    else Out2.apply(norms, input.concat(materialization(norms)), throughput)
  }
}
