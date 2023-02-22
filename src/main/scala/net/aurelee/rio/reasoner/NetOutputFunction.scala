package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, mkConjs, mkDisjs}

sealed abstract class NetOutputFunction {
  def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula]
}
case object CredulousNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] = Seq.concat(outFamily:_*)
}
case object SkepticalNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] = Seq(mkDisjs(outFamily.map(mkConjs)))
}
