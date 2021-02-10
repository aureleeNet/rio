package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, Norm, OutOperator, PLTop, interreduce, mkConjs}
import net.aurelee.rio.sat.consequence

object Out4 extends OutOperator {
  override final def name: String = "out4"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) Out2.apply(norms, input, throughput)
    else {
      var lastOutput: Seq[Formula] = Seq(PLTop)
      var basicOutput = Out2.apply(norms, input, throughput)
      var triggeredOutput = basicOutput.concat(Out3.apply(norms, input.concat(basicOutput), throughput))

      while (!consequence(lastOutput, mkConjs(triggeredOutput))) {
        lastOutput = triggeredOutput
        basicOutput = triggeredOutput.concat(Out2.apply(norms, lastOutput, throughput))
        triggeredOutput = basicOutput.concat(Out3.apply(norms, input.concat(basicOutput), throughput))
      }
      interreduce(lastOutput)
    }
  }
}
