package net.aurelee.rio

import net.aurelee.rio.core.{Formula, Norm, OutOperator, bodies, body, head, mkDisj, mkDisjs, mkImpl, mkNeg}

package object reasoner {
  import sat.globalPicoSATInstance

  sealed abstract class ConstrainedSetting
  final case object ConstrainedCredulous extends ConstrainedSetting
  final case object ConstrainedSkeptical extends ConstrainedSetting

  final case class RioConfig(operator: OutOperator, throughput: Boolean, constrained: Option[ConstrainedSetting])

  final def materialization(norms: Seq[Norm]): Seq[Formula] = {
    norms.map(n => mkImpl(body(n), head(n)))
  }

  final def getDirectlyTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    import sat.consequence
    norms.filter(n => consequence(input, body(n)))
  }

  final def getBasicTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    import sat.consequence
    norms.filter { n =>
      val compat = getCompatibleNorms(norms,n)
      val compatBodies = bodies(compat)
      val bigDisjunction = mkDisjs(compatBodies)
      consequence(input, bigDisjunction)
    }
  }

  final def getCompatibleNorms(norms: Seq[Norm], n: Norm): Seq[Norm] = {
    import net.aurelee.rio.sat.consequence
    val h = head(n)
    norms.filter(m => consequence(Seq(head(m)), h))
  }

}
