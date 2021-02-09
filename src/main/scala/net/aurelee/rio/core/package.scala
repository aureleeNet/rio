package net.aurelee.rio

package object core {
  sealed abstract class ConstrainedSetting
  final case object ConstrainedCredulous extends ConstrainedSetting
  final case object ConstrainedSkeptical extends ConstrainedSetting

  final case class RioConfig(operator: OutOperator, throughput: Boolean, constrained: Option[ConstrainedSetting])
}
