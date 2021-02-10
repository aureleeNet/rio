package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, interreduce, mkConjs, mkDisjs}

sealed abstract class NetOutputFunction {
  def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula]
}
case object CredulousNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] = interreduce(Seq.concat(outFamily:_*))
}
case object SkepticalNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] = {
    interreduce(Seq(mkDisjs(outFamily.map(mkConjs))))
  }
}
