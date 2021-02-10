package net.aurelee.rio.core

trait OutOperator {
  def name: String
  def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula]
}

